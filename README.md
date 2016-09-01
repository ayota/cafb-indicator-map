# CAFB-health-microestimates

## Background

DataKindDC and the Capital Area Food Bank are attempting to generate more geographically detailed risk factor scores for diabetes, obesity, and heart disease rate in the United States. Estimates for these health conditions are currently available at the county level from the Center for Disease Control (CDC) but DKDC and CAFB are trying to develop a model to estimate a risk factor at the Census tract and block level. 

## Method

Using the CDC Behavioral Risk Factor Survelliance System (BRFSS) dataset from 2014, one can develop a model to associate demographic characteristics with the incidence of diabetes. By using American Communities Survey (ACS) data from the U.S. Census, those estimates can be applied to tract and block level demographics to generate risk factor scores for tracts and blocks. 

The script `diabetes_model.R` runs a logistic model from the BRFSS data to get probabilities of diabetes occurrence based on age, sex, race, income level, and separate intercepts for each US state. It then applies those probabilities to a synthetic population derived from the PUMS and ACS data for each census tract (see the `ipf` folder for details of that process), in order to produce estimates of prevalence of diabetes (numbers and proportion) per census tract (output in `diabetes_estimates.csv`).