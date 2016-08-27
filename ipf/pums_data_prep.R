library(dplyr)

# PUMS (microdata) files downloaded from:
# http://www2.census.gov/programs-surveys/acs/data/pums/2014/5-Year/
#  (e.g. for MD, person file is ss14pmd.csv, household is ss14hmd.csv) 


# Process PUMS data for given state (lowercase abbreviation)
process_pums <- function(state_abb) {
    # Load person file and subset variables
    pums <- read.csv(paste0("ipf/ss14p", state_abb, ".csv"))
    pums <- dplyr::select(pums, ST, PUMA10, PWGTP, 
                          AGEP, SEX, RAC1P, FHISP, SERIALNO)
    
    # Get household income from household file
    pums_hh <- read.csv(paste0("ipf/ss14h", state_abb, ".csv"))
    pums_hh <- dplyr::select(pums_hh, SERIALNO, HINCP, ADJINC)
    pums <- left_join(pums, pums_hh)
    
    # Filter out records with no PUMA10 (-9 is NA)
    pums <- filter(pums, PUMA10 != -9)
    
    # Convert variables to factors with BRFSS cutpoints
    pums$sex <- factor(pums$SEX, labels = c("male", "female"))
    pums$age <- cut(pums$AGEP, breaks = c(-1, 18, seq(25, 80, 5), 100), right = FALSE,
                       labels = c("0_17", "18_24", "25_29", "30_34", "35_39", "40_44", 
                                  "45_49", "50_54", "55_59", "60_64", "65_69", 
                                  "70_74", "75_79", "80_plus"))
    pums$race <- as.character(cut(pums$RAC1P, breaks = c(0, 1, 2, 5, 7, 9),
                                  labels = c("white", "black", "native_american", 
                                  "asian_pacific_islander", "other_multiracial")))
    # Hispanic ethnicity is a category of race in BRFSS
    pums$race[pums$FHISP == 1] <- "hispanic"
    pums$income <- as.numeric(pums$HINCP) * pums$ADJINC * 1E-6 # inflation adjustment
    pums$income <- cut(pums$income, breaks = c(-99999, 10000, 15000, 20000,
                                                25000, 35000, 50000, 1E9),
                          labels = c("under_10k", "10k_15k", "15k_20k", "20k_25k", 
                                     "25k_35k", "35k_50k", "over_50k"))   
    
    dplyr::select(pums, statefp = ST, puma = PUMA10, wgt = PWGTP, 
                  sex, age, race, income)
}

states <- c("dc", "md", "va")

pums_dcmdva <- bind_rows(lapply(states, process_pums)) 

# Tally unique combinations of variables and output file
pums_counts <- group_by(pums_dcmdva, statefp, puma, sex, age, race, income) %>%
    summarize(count = sum(wgt))

write.csv(pums_counts, "ipf/pums_2012_2014.csv", row.names = FALSE)


# Household data (for household income / size relationship)

get_pums_hh <- function(state_abb) {
    # Get household size, income and weight data
    pums_hh <- read.csv(paste0("ipf/ss14h", state_abb, ".csv"))
    pums_hh <- dplyr::select(pums_hh, ST, PUMA10, WGTP, NP, HINCP, ADJINC)
    
    # Only keep records with PUMA10, non-zero weight and non-vacant housing
    pums_hh <- filter(pums_hh, PUMA10 != -9, WGTP > 0, NP > 0)
    
    pums_hh$income <- as.numeric(pums_hh$HINCP) * pums_hh$ADJINC * 1E-6 # inflation adjustment
    pums_hh$income <- cut(pums_hh$income, breaks = c(-99999, 10000, 15000, 20000,
                                                     25000, 35000, 50000, 1E9),
                          labels = c("under_10k", "10k_15k", "15k_20k", "20k_25k", 
                                     "25k_35k", "35k_50k", "over_50k"))
    
    dplyr::select(pums_hh, statefp = ST, puma = PUMA10, wgt = WGTP,
                  size = NP, income)
}

pums_hh <- bind_rows(lapply(states, get_pums_hh)) 

pums_hh <- group_by(pums_hh, statefp, puma, size, income) %>%
    summarize(count = sum(wgt))

write.csv(pums_hh, "ipf/pums_hh_2012_2014.csv", row.names = FALSE)

