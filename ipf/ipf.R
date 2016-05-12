library(tidyr)
library(dplyr)
library(mipfp)
library(reshape2)

# Load PUMS data, ACS data and PUMA/census tract mapping
pums <- read.csv("ipf/pums_2012_2014.csv", stringsAsFactors = FALSE)
acs_sex_age <- read.csv("ipf/acs2014_sex_age.csv", stringsAsFactors = FALSE)
acs_race <- read.csv("ipf/acs2014_race.csv", stringsAsFactors = FALSE)
acs_income <- read.csv("ipf/acs2014_income.csv", stringsAsFactors = FALSE)
tract_puma <- read.csv("ipf/tract_puma_map.csv", stringsAsFactors = FALSE)
colnames(tract_puma) <- c("statefp", "countyfp", "tract", "puma")

# Get mean household size per PUMA and income level
pums_hh <- read.csv("ipf/pums_hh_2012_2014.csv", stringsAsFactors = FALSE)
pums_hh <- group_by(pums_hh, statefp, puma, income) %>%
    summarize(mean_size = weighted.mean(size, count))

# Use it to adjust acs_income counts (from households to people)
acs_income <- inner_join(acs_income, tract_puma) %>%
    left_join(pums_hh) %>%
    mutate(count = count * mean_size) %>%
    dplyr::select(-puma, -mean_size)

# Correct so that total counts match by tract
tract_counts <- group_by(acs_race, statefp, countyfp, tract) %>%
    summarize(count = sum(count))
tract_inc_counts <- group_by(acs_income, statefp, countyfp, tract) %>%
    summarize(inc_count = sum(count))
tract_counts <- inner_join(tract_counts, tract_inc_counts) %>%
    mutate(adj = count / inc_count)

# NOTE: There are large discrepancies for some census tracts, mostly because of group
# quarters (e.g. colleges, prisons) that are included in ACS but are not households


# Example IPF with one PUMA

# Subset PUMS to selected state and PUMA, then recast to 4D matrix
pums1 <- filter(pums, statefp == 11, puma == 101) %>%
    group_by(sex, age, race, income) %>%
    summarize(count = sum(count)) %>%
    filter(!is.na(income))  # remove cells with NA income

pums_arr <- acast(pums1, sex ~ age ~ race ~ income, fill = 0)

# This function subsets an ACS table to tracts that are in the selected state and PUMA
#  it also concatenates state, county and tract into tract_id
subset_acs <- function(acs_table, sel_state, sel_puma) {
    inner_join(acs_table, tract_puma) %>%
        filter(statefp == sel_state, puma == sel_puma) %>%
        unite(tract_id, statefp, countyfp, tract, sep = "_") %>%
        dplyr::select(-puma)
}

# Subset each ACS table to PUMA and recast to array
sa_arr <- subset_acs(acs_sex_age, 11, 101) %>%
    acast(tract_id ~ sex ~ age)
race_arr <- subset_acs(acs_race, 11, 101) %>% 
    acast(tract_id ~ race)
inc_arr <- subset_acs(acs_income, 11, 101) %>%
    acast(tract_id ~ income)


# Note: not using income for now due to data issues mentioned above...
pums_no_inc <- apply(pums_arr, 1:3, sum)


# First step IPF: PUMS with totals across all tracts
sa_totals <- apply(sa_arr, 2:3, sum)
race_totals <- apply(race_arr, 2, sum)

ipf1 <- Ipfp(seed = pums_no_inc, target.list = list(1:2, 3),
             target.data = list(sa_totals, race_totals))
ipf1 <- ipf1$x.hat

# Second step IPF: use ipf1 and original arrays as marginals of 
#  a new array with census tract as an additional variable
init_arr <- array(1, dim = c(dim(sa_arr)[1], dim(pums_no_inc)),
                  dimnames = c(dimnames(sa_arr)[1], dimnames(pums_no_inc)))

ipf2 <- Ipfp(seed = init_arr, target.list = list(1:3, c(1,4), 2:4),
             target.data = list(sa_arr, race_arr, ipf1))

# Get proportions of each demographic combination, per tract, as a data frame
prop_df <- melt(ipf2$p.hat, varnames = c("tract", "sex", "age", "race"))
