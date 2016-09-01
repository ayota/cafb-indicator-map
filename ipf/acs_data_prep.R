library(acs)
library(tidyr)
library(dplyr)
library(stringr)

# Make geolist of all tracts in specified counties
states_counties <- read.csv("ipf/states_counties.csv")
tracts <- geo.make(state = states_counties$state_fips,
                   county = states_counties$county_fips, tract = "*")

# Get tables B01001 (sex by age), B03002 (race/ethnicity) and B19001 (income)
# (note: need to set CENSUS_KEY environmental variable to valid census API key)
sa_tab <- acs.fetch(2014, table.number = "B01001", geography = tracts,
                   key = Sys.getenv("CENSUS_KEY")) 
race_tab <- acs.fetch(2014, table.number = "B03002", geography = tracts,
                      key = Sys.getenv("CENSUS_KEY"))
inc_tab <- acs.fetch(2014, table.number = "B19001", geography = tracts,
                    key = Sys.getenv("CENSUS_KEY"))

# Build state_county_tract IDs and add to data tables
geog_df <- sa_tab@geography
tract_id <- paste(geog_df$state, geog_df$county, geog_df$tract, sep = "_")
sa_df <- cbind(tract_id, data.frame(sa_tab@estimate))
race_df <- cbind(tract_id, data.frame(race_tab@estimate))
inc_df <- cbind(tract_id, data.frame(inc_tab@estimate))

# Total pop. by census tract
pop_by_tract <- dplyr::select(sa_df, tract_id, pop = B01001_001)

# Reshape data tables to 'tall' form and map variable names to BRFSS categories
sa_df <- gather(sa_df, key = "var", value = "count", -tract_id)
sa_df$sex_age <- as.integer(str_sub(sa_df$var, -3, -1))
sa_map <- read.csv("ipf/sex_age_map.csv")
sa_df <- left_join(sa_df, sa_map) %>% 
    dplyr::select(tract_id, sex, age, count) %>%
    filter(!is.na(sex), !is.na(age)) %>%
    group_by(tract_id, sex, age) %>%
    summarize(count = sum(count))

race_df <- gather(race_df, key = "var", value = "count", -tract_id)
race_df$race_code <- as.integer(str_sub(race_df$var, -3, -1))
race_map <- read.csv("ipf/race_map.csv")
race_df <- left_join(race_df, race_map) %>%
    dplyr::select(tract_id, race, count) %>%
    filter(!is.na(race)) %>%
    group_by(tract_id, race) %>%
    summarize(count = sum(count))

inc_df <- gather(inc_df, key = "var", value = "count", -tract_id)
inc_df$income_code <- as.integer(str_sub(inc_df$var, -3, -1))
inc_map <- read.csv("ipf/income_map.csv")
inc_df <- left_join(inc_df, inc_map) %>%
    dplyr::select(tract_id, income, count) %>%
    filter(!is.na(income)) %>%
    group_by(tract_id, income) %>%
    summarize(count = sum(count))

# Separate tract_id back into state, county and tract, then save
sa_df <- separate(sa_df, tract_id, c("statefp", "countyfp", "tract"), 
                  sep = "_", convert = TRUE)
write.csv(sa_df, "ipf/acs2014_sex_age.csv", row.names = FALSE)

race_df <- separate(race_df, tract_id, c("statefp", "countyfp", "tract"),
                    sep = "_", convert = TRUE)
write.csv(race_df, "ipf/acs2014_race.csv", row.names = FALSE)

inc_df <- separate(inc_df, tract_id, c("statefp", "countyfp", "tract"), 
                  sep = "_", convert = TRUE)
write.csv(inc_df, "ipf/acs2014_income.csv", row.names = FALSE)

# Get group quarter population by tract, merge with total pop. and export
gq_tab <- acs.fetch(2014, table.number = "B26001", geography = tracts,
                    key = Sys.getenv("CENSUS_KEY")) 
gq_df <- cbind(tract_id, data.frame(gq_tab@estimate))
pop_by_tract <- inner_join(pop_by_tract, rename(gq_df, gq_pop = B26001_001))

pop_by_tract <- separate(pop_by_tract, tract_id, c("statefp", "countyfp", "tract"),
                         sep = "_", convert = TRUE)
write.csv(pop_by_tract, "ipf/acs2014_pop.csv", row.names = FALSE)

# Get household size data (from table B11016)
hhsize_tab <- acs.fetch(2014, table.number = "B11016", geography = tracts,
                        key = Sys.getenv("CENSUS_KEY"))
hhsize_df <- cbind(tract_id, data.frame(hhsize_tab@estimate))
hhsize_df <- gather(hhsize_df, key = "var", value = "count", -tract_id)
hhsize_df$size_code <- as.integer(str_sub(hhsize_df$var, -3, -1))
hhsize_map <- read.csv("ipf/hhsize_map.csv")
hhsize_df <- left_join(hhsize_df, hhsize_map) %>%
    dplyr::select(tract_id, hhsize, count) %>%
    filter(!is.na(hhsize)) %>%
    group_by(tract_id, hhsize) %>%
    summarize(count = sum(count))

hhsize_df <- separate(hhsize_df, tract_id, c("statefp", "countyfp", "tract"),
                    sep = "_", convert = TRUE)
write.csv(hhsize_df, "ipf/acs2014_hhsize.csv", row.names = FALSE)
