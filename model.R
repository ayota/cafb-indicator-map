library(dplyr)

#importing BRFSS Data that are cleaned 
brfssRaw <- read.csv('brfss2014clean.csv', na.strings = "")

#Coding the health outcomes
brfss <- mutate(brfssRaw,
                heart_attack = as.integer(heart_attack) - 1, 
                heart_disease = as.integer(heart_disease) - 1,
                diabetes = as.integer(diabetes) - 1)

#Keeping only records that are complete with respect to the variables in question
brfss <- filter(brfss,
                !is.na(age),
                !is.na(sex),
                !is.na(income),
                !is.na(race),
                !is.na(diabetes),
                !is.na(heart_disease),
                !is.na(heart_attack),
                !is.na(state))

#Transposing the factor data from long to wide
brfss <- cbind(brfss, model.matrix(~ brfss$age - 1))
brfss <- cbind(brfss, model.matrix(~ brfss$sex - 1))
brfss <- cbind(brfss, model.matrix(~ brfss$race - 1))
brfss <- cbind(brfss, model.matrix(~ brfss$income - 1))

#Editing the factor names from the BRFSS data to be machine readable friendly
x <- names(brfss)
x <- gsub('brfss\\$','',x)
x <- gsub("-","_",x)
x <- gsub("<","less_than",x)
x <- gsub('>','greater_than',x)
x <- gsub('/','slash',x)
x <- gsub('\\+', 'plus',x)
x <- gsub(' ', '',x)
names(brfss) <- x

#Running a linear model
modelLogit <- glm(diabetes ~ 
                    #name  
                    age18_24 
                  + age25_29 
                  + age30_34 
                  + age40_44 
                  + age50_54 
                  + age60_64 
                  + age70_74 
                  + income10k_15k 
                  + income20k_25k 
                  + income25k_35k
                  + income35k_50k
                  + sexfemale
                  + racewhite
                  + raceblack
                  + racenativeamerican
                  + raceasianslashpacificislander
                  + raceotherslashmultiracial
                  , data = brfss)#, family = "binomial")

#Reading in the ACS and FARA data that have been massaged to be percentages
acs <- read.csv('acs_FARA_combined_counts.csv', stringsAsFactors=FALSE)
y <- names(acs)
y <- gsub("\\.","",y)
names(acs) <- y
State_FIPS <- c(11,51,24)
name <- c('Washington D.C.','Virginia','Maryland')
xwalk <- as.data.frame(cbind(State_FIPS,name))
xwalk$State_FIPS <- as.integer(xwalk$State_FIPS)
acs <- left_join(acs, xwalk, by='State_FIPS')

#Using the census tract level data as inputs to predict from the above model
diabetesRiskScore <- as.data.frame(cbind(acs$CensusTract,predict(modelLogit,newdata=acs,na.omit=TRUE)))
names(diabetesRiskScore) <- c('CensusTract','diabetesRiskScore')

#Write the diabetes risk score
write.csv(diabetesRiskScore,'diabetesRiskScore.csv')



