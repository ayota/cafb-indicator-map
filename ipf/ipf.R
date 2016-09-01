library(tidyr)
library(dplyr)
library(mipfp)
library(reshape2)
library(stringr)


# Load PUMS data, ACS data and PUMA/census tract mapping
pums <- read.csv("ipf/pums_2012_2014.csv", stringsAsFactors = FALSE)
pums_hh <- read.csv("ipf/pums_hh_2012_2014.csv", stringsAsFactors = FALSE)
acs_sex_age <- read.csv("ipf/acs2014_sex_age.csv", stringsAsFactors = FALSE)
acs_race <- read.csv("ipf/acs2014_race.csv", stringsAsFactors = FALSE)
acs_income <- read.csv("ipf/acs2014_income.csv", stringsAsFactors = FALSE)
acs_hhsize <- read.csv("ipf/acs2014_hhsize.csv", stringsAsFactors = FALSE)
acs_pop <- read.csv("ipf/acs2014_pop.csv", stringsAsFactors = FALSE)
tract_puma <- read.csv("ipf/tract_puma_map.csv", stringsAsFactors = FALSE)
colnames(tract_puma) <- c("statefp", "countyfp", "tract", "puma")

# Filter PUMA to only those covering census tracts of interest
tract_puma <- semi_join(tract_puma, acs_pop)
pums <- semi_join(pums, tract_puma)
pums_hh <- semi_join(pums_hh, tract_puma)


# Estimate number of individuals by tract and income level ----

# Save mean size of 7+ people household in PUMS, 
#  and reduce to one category to match ACS
mean_7plus <- mean(pums_hh$size[pums_hh$size >= 7])
pums_hh$size[pums_hh$size >= 7] <- 7
pums_hh <- group_by(pums_hh, statefp, puma, size, income) %>%
    summarize(count = sum(count))

# Estimate HH size x income matrix by census tract, via IPF

# This function subsets an ACS table to tracts that are in the selected state and PUMA
#  it also concatenates state, county and tract into tract_id
subset_acs <- function(acs_table, sel_state, sel_puma) {
    inner_join(acs_table, tract_puma) %>%
        filter(statefp == sel_state, puma == sel_puma) %>%
        unite(tract_id, statefp, countyfp, tract, sep = "_") %>%
        dplyr::select(-puma)
}

ipf_size_income <- function(sel_state, sel_puma) {
    pums_sel <- filter(pums_hh, statefp == sel_state, puma == sel_puma)
    pums_arr <- acast(pums_sel, size ~ income, fill = 0)
    size_arr <- subset_acs(acs_hhsize, sel_state, sel_puma) %>%
        acast(tract_id ~ hhsize)
    inc_arr <- subset_acs(acs_income, sel_state, sel_puma) %>%
        acast(tract_id ~ income)

    # IPF step 1    
    size_totals <- apply(size_arr, 2, sum)
    inc_totals <- apply(inc_arr, 2, sum)
    ipf1 <- Ipfp(seed = pums_arr, target.list = list(1, 2),
                 target.data = list(size_totals, inc_totals))
    ipf1 <- ipf1$x.hat
    
    # IPF step 2
    init_arr <- array(1, dim = c(dim(size_arr)[1], dim(pums_arr)),
                      dimnames = c(dimnames(size_arr)[1], dimnames(pums_arr)))
    ipf2 <- Ipfp(seed = init_arr, target.list = list(1:2, c(1,3), 2:3),
                 target.data = list(size_arr, inc_arr, ipf1))
    
    melt(ipf2$x.hat, varnames = c("tract_id", "hhsize", "income"))
}

state_puma <- unique(dplyr::select(ungroup(pums_hh), statefp, puma))

size_inc_est <- bind_rows(Map(ipf_size_income, 
                              state_puma$statefp, state_puma$puma))

# Replace 7+ hhsize with mean found above
size_inc_est$hhsize[size_inc_est$hhsize == 7] <- mean_7plus

# Get number of individuals by tract and income level
acs_income_est <- group_by(size_inc_est, tract_id, income) %>%
    summarize(count = sum(hhsize * value))

acs_income_est <- separate(acs_income_est, tract_id, 
                           c("statefp", "countyfp", "tract"), sep = "_", 
                           convert = TRUE)

# Correct estimates so that totals match tract population (excl. group quarter)
acs_income_adj <- group_by(acs_income_est, statefp, countyfp, tract) %>%
    summarize(count = sum(count)) %>%
    inner_join(acs_pop) %>%
    mutate(adj = count / (pop - gq_pop))

acs_income_est <- inner_join(acs_income_est, 
                             dplyr::select(acs_income_adj, statefp, countyfp,
                                           tract, adj)) %>%
    mutate(new_count = ifelse(is.na(adj), count, count / adj))

acs_income_est <- dplyr::select(acs_income_est, statefp, countyfp, tract,
                                income, count = new_count)    

# Add group quarters population (with no HH income) as separate category
acs_income_gq <- cbind(dplyr::select(acs_pop, statefp, countyfp, tract),
                       income = "GQ", dplyr::select(acs_pop, count = gq_pop))
acs_income_est <- bind_rows(acs_income_est, acs_income_gq)

# Replace NA income (individuals in group quarters) with "GQ" in pums
pums$income[is.na(pums$income)] <- "GQ"


# Create four-dimension (sex, age, race, income) count matrix by tract  -------

# Remove tracts with low population (< 100)
tracts_rem <- filter(acs_pop, pop < 100)
tract_puma <- anti_join(tract_puma, tracts_rem)

# Main function for creation of synthetic population via IPF
ipf_4D <- function(sel_state, sel_puma) {
    # Subset PUMS and recast to 4D array
    pums_sel <- filter(pums, statefp == sel_state, puma == sel_puma)
    pums_arr <- acast(pums_sel, sex ~ age ~ race ~ income, fill = 0)

    # Subset each ACS table to PUMA and recast to array
    sa_arr <- subset_acs(acs_sex_age, sel_state, sel_puma) %>%
        acast(tract_id ~ sex ~ age)
    race_arr <- subset_acs(acs_race, sel_state, sel_puma) %>% 
        acast(tract_id ~ race)
    inc_arr <- subset_acs(acs_income_est, sel_state, sel_puma) %>%
        acast(tract_id ~ income)
    
    # IPF step 1: PUMS with totals across all tracts    
    sa_totals <- apply(sa_arr, 2:3, sum)
    race_totals <- apply(race_arr, 2, sum)
    inc_totals <- apply(inc_arr, 2, sum)
    ipf1 <- Ipfp(seed = pums_arr, target.list = list(1:2, 3, 4),
                 target.data = list(sa_totals, race_totals, inc_totals), tol = 0.01)
    ipf1 <- ipf1$x.hat
    
    # IPF step 2: Use ipf1 and original arrays as marginals of 
    #  a new array with census tract as an additional variable
    init_arr <- array(1, dim = c(dim(sa_arr)[1], dim(pums_arr)),
                      dimnames = c(dimnames(sa_arr)[1], dimnames(pums_arr)))
    ipf2 <- Ipfp(seed = init_arr, target.list = list(1:3, c(1,4), c(1,5), 2:5),
                 target.data = list(sa_arr, race_arr, inc_arr, ipf1), tol = 0.01)
    
    # Get counts of each demographic combination, per tract, as data frame
    melt(ipf2$x.hat, varnames = c("tract_id", "sex", "age", "race", "income"))
}

# Apply function above to all PUMAs
synth_pop <- bind_rows(Map(ipf_4D, state_puma$statefp, state_puma$puma))

# Remove rows with no counts
synth_pop <- rename(synth_pop, count = value) %>%
    filter(count > 0)

# Remove minors and residents of group quarters
synth_pop <- filter(synth_pop, age != "0_17", income != "GQ")

# Export results to .csv
write.csv(synth_pop, "synth_pop.csv", row.names = FALSE)
