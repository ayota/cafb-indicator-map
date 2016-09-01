library(tidyr)
library(dplyr)
library(stringr)

# Import the clean BRFSS data 
brfssRaw <- read.csv("brfss2014clean.csv", na.strings = "")

# Import synthetic population generated for each census tract via IPF
synth_pop <- read.csv("synth_pop.csv")
synth_pop <- separate(synth_pop, tract_id, c("statefp", "countyfp", "tract"),
                      sep = "_", convert = TRUE)

# Code the health outcomes
brfss <- mutate(brfssRaw,
                heart_attack = as.integer(heart_attack) - 1, 
                heart_disease = as.integer(heart_disease) - 1,
                diabetes = as.integer(diabetes) - 1)

# Keep only records that are complete with respect to the variables in question
brfss <- filter(brfss,
                !is.na(age),
                !is.na(sex),
                !is.na(income),
                !is.na(race),
                race != "refused/unknown",
                !is.na(diabetes),
                !is.na(state))

# Make labels match between brfss and synth_pop
brfss <- droplevels(brfss)
levels(brfss$age) <- levels(synth_pop$age)
levels(brfss$race) <- levels(synth_pop$race)
synth_pop$income <- relevel(synth_pop$income, "under_10k")
levels(brfss$income) <- levels(synth_pop$income)

state_names <- data.frame(statefp = c(11, 24, 51),
                          name = c("Washington D.C.", "Maryland", "Virginia"),
                          stringsAsFactors = FALSE)
synth_pop <- inner_join(synth_pop, state_names)
synth_pop$name <- factor(synth_pop$name, levels = levels(brfss$name))
levels(synth_pop$name) <- levels(brfss$name)


# Run a logistic regression of diabetes occurrence from BRFSS data
logitmod <- glm(diabetes ~ name + age + sex + race + income, 
                  family = "binomial", data = brfss)

# Use model to predict prevalence in synthetic population
diabetes_pred <- predict(logitmod, synth_pop, type = "response")
synth_pop <- cbind(synth_pop, diabetes_pred)

# Total among all demographic groups by tract
synth_pop_sum <- group_by(synth_pop, statefp, countyfp, tract) %>%
    summarize(pop = sum(count), diabetes_num = sum(count * diabetes_pred),
              diabetes_frac = diabetes_num / pop)

# Output results to file
write.csv(synth_pop_sum, "diabetes_estimates.csv", row.names = FALSE)


