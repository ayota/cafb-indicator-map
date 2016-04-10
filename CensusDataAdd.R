#Load Libraries
library(httr)
library(jsonlite)
library(RCurl)



#Initialize variables/values
key = ##INSERT YOUR ACS KEY HERE##
acsTables <- read.csv('acsTables.csv', stringsAsFactors = FALSE)
fips = c(11,24,51)
## 11 - DC, 24 - MD, 51 - VA

#initialize data frames
acs = data.frame()
acsTemp = data.frame()
monitor = data.frame()

#For loop to populate dataframe
#Run loop THREE TIMES, once for each FIPS code


for ( i in 1:nrow(acsTables)) {
  
  varName <- acsTables[i,1]

  ##Update FIPS index in URL below for each run
url <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',varName,'&for=tract:*&in=state:',fips[3],'*&key=',key)

acsTemp <- as.data.frame(fromJSON(getURL(url)))
acsTemp <- acsTemp[-1,]
names(acsTemp) <- c('geoName','value', 'State_FIPS', 'County_FIPS', 'Tract_FIPS')
acsTemp$shortName   <- acsTables[i,6] 

##optional, can be included to measure progress
##monitor <- rbind(monitor, varName)  
print(varName)  
##End optional code

acs <- rbind(acs,acsTemp)  

}


#check output
head(acs)
tail(acs)


#publish file
write.csv(acs, file = "DCVAMD_ACS.csv")









