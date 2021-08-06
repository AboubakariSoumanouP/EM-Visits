library(rvest)
library(sf)
library(tidyverse)
library(jsonlite)
library(tidycensus)
# Reading NYC Open Data on Emergency Visits
emergencyVisits <- fromJSON("https://data.cityofnewyork.us/resource/2nwg-uqyg.json?$limit=1000000")

# Converting the dates to proper dates
emergencyVisits$goodDates <- as.Date(emergencyVisits$date)

# Question 1: Get only the last N days
Sample <-emergencyVisits[sample(1:nrow(emergencyVisits), 100),]



max(emergencyVisits$goodDates)
(recentEmergencyVisits<-max(emergencyVisits$goodDates))
N=100
Nrecent <- emergencyVisits%>%filter(goodDates>= recentEmergencyVisits-N)
Nrecent$total_ed_visits = strtoi(Nrecent$total_ed_visits)

# Question 2: Calculate the Number of Visits per ZIP

visitsByZip = Nrecent %>%
  group_by(mod_zcta) %>%
  summarize(Number_of_Visits= sum(total_ed_visits))

# glimpse(visitsByZip)

#Question 3: Graph a map outlining the ZIP codes of NYC

loc = "C:/Users/Aboubakari Soumanou/Downloads"
NYCmap = st_read(loc, layer = "ZIP_CODE_040114")
plot(st_geometry(NYCmap))

# glimpse(NYCmap)
NYCmap<-NYCmap%>%select(-BLDGZIP)
NYCmap<-NYCmap%>%select(-PO_NAME)
NYCmap<-NYCmap%>%select(-POPULATION)
NYCmap<-NYCmap%>%select(-AREA)
NYCmap<-NYCmap%>%select(-STATE)
NYCmap<-NYCmap%>%select(-COUNTY)
NYCmap<-NYCmap%>%select(-ST_FIPS)
NYCmap<-NYCmap%>%select(-URL)
NYCmap<-NYCmap%>%select(-SHAPE_AREA)
NYCmap<-NYCmap%>%select(-SHAPE_LEN)
# glimpse(NYCmap)
# Question 4: Ensure the zip codes in both dataframes are in columns with identical names


NYCMapZip = NYCmap %>%
  mutate(mod_zcta = as.character(ZIPCODE)) %>%
  select(mod_zcta, geometry)

# glimpse(NYCMapZip)
# glimpse(visitsByZip)
# Question 5: Join the Map and ED Visits datasets

NYCZipAndvisits = inner_join(x = NYCMapZip, y= visitsByZip)
# glimpse(NYCZipAndvisits)
# Question 6: Map the number of visits in each ZIP code
plot(NYCZipAndvisits["Number_of_Visits"])

census_api_key("aa512886c5449a582d837da8d3a07af66a043fe5", 
               install=TRUE, overwrite=T)
censusData = load_variables(2018, "acs5", cache=T)

readRenviron("~/.Renviron")
populationData = get_acs(geography = "zcta",
                         variables = 'B01003_001',
                         geometry = FALSE)

populationData$mod_zcta = sub("ZCTA5 ", "", populationData$NAME)

# Question 7: Rename the Population Column and Clean Up Your Dataframe
populationData=rename(populationData, population = estimate)

# Question 8: Remove the columns we don't need anymore

populationData<-populationData%>%select(-variable)
populationData<-populationData%>%select(-NAME)
# glimpse(populationData)

#Question 9: Now that ZIP codes are cleaned up, merge (inner join) the population data set
#with your ED dataframe

PopulationAndvisits = inner_join(x=NYCZipAndvisits,y =populationData)
# glimpse(PopulationAndvisits)

# Question 10: Now that you have, for each ZIP code, their counts of ED visits and their
# populations, create a column that reports the rate of Emergency visits per inhabitant
PopulationAndvisits<-mutate(PopulationAndvisits, 
                            rate = (Number_of_Visits)/ (population))
# glimpse(PopulationAndvisits)

vars = c(poverty = 'B17001_002')
povertyData = get_acs(geography = "zcta",
                      variables = vars,
                      geometry = FALSE)

# Question 11: Remove the spurious "ZCTA5" from the ZIP data & Clean-Up
povertyData$NAME = sub("ZCTA5 ", "", povertyData$NAME)
povertyData$mod_zcta = sub("ZCTA5 ", "", povertyData$NAME)
povertyData=rename(povertyData, Number = estimate)

povertyData<-povertyData%>%select(-moe)
povertyData<-povertyData%>%select(-NAME)
povertyData<-povertyData%>%select(-variable)

# glimpse(povertyData)
# glimpse(PopulationAndvisits)
# Question 12: Perform an inner join of the dataframe you had with the poverty data just
# downloaded
PoVisitsandPoverty = inner_join(x= PopulationAndvisits,y =povertyData)
# glimpse(PoVisitsandPoverty)

# Question 13: Calculate the poverty rate and the rate of visits per inhabitant
PoVisitsandPoverty <- mutate(PoVisitsandPoverty, 
                             PovertyRate = (Number)/ (population))
# glimpse(PoVisitsandPoverty) 

PoVisitsandPoverty <- mutate(PoVisitsandPoverty, 
                             RateOfVisits = (Number_of_Visits)/ (Number))
# glimpse(PoVisitsandPoverty) 

# Question 14: Analyze the linear relationship between poverty rate and 
# the number of visits, across ZIP codes
plot(PoVisitsandPoverty$PovertyRate, PoVisitsandPoverty$Number_of_Visits, 
     xlab="PovertyRate", ylab="Number_of_Visits")
m<-lm(PoVisitsandPoverty$Number_of_Visits~PoVisitsandPoverty$PovertyRate)
abline(m,col="red")
summary(m)$adj.r.squared
legend("topright",legend=paste("R-squared= ",
                               format(summary(m)$adj.r.squared,digits=2)))

#  We conclude that there is a positive and week relationship between 
# poverty rate and the number of visits, across ZIP codes

# Question 15: Analyze the linear relationship between poverty rate and 
# the rate of visits per inhabitant, across ZIP codes

plot(PoVisitsandPoverty$PovertyRate, PoVisitsandPoverty$RateOfVisits, 
     xlab="PovertyRate", ylab="RateOfVisits")
mb<-lm(PoVisitsandPoverty$RateOfVisits~PoVisitsandPoverty$PovertyRate)
abline(mb,col="red")
summary(mb)$adj.r.squared
legend("topright",legend=paste("R-squared= ",
                               format(summary(mb)$adj.r.squared,digits=2)))

#  We conclude that there is a week and negative relationship between poverty 
# rate and the rate of visits per inhabitant, across ZIP codes


# Question 16: Rename the "estimate" column, remove "ZCTA5" from the ZIP codes, 
# and clean up the dataframevars = c(age = 'B01002_001')
ageData = get_acs(geography = "zcta", variables = vars, geometry = FALSE)

ageData=rename(ageData, AgeNumber = estimate)
ageData$mod_zcta = sub("ZCTA5 ", "", ageData$NAME)
glimpse(ageData)
ageData<-ageData%>%select(-moe)
ageData<-ageData%>%select(-GEOID)

# glimpse(ageData)
# glimpse(PoVisitsandPoverty)

# Question 17: Merge (inner join) with the main dataset
AgeandPopulation = inner_join(x= PoVisitsandPoverty, y =ageData)
# glimpse(AgeandPopulation)


AgeandPopulation<-AgeandPopulation%>%select(-moe)
AgeandPopulation<-AgeandPopulation%>%select(-GEOID)
# glimpse(AgeandPopulation)

# Question 18: Analyse the relationship between age and ED visits
plot(AgeandPopulation$AgeNumber, AgeandPopulation$Number_of_Visits, 
     xlab="AgeNumber", ylab="Number_of_Visits")
mb<-lm(AgeandPopulation$Number_of_Visits~AgeandPopulation$AgeNumber)
abline(mb,col="red")
summary(mb)$adj.r.squared
legend("topright",legend=paste("R-squared= ",
                               format(summary(mb)$adj.r.squared,digits=2)))

# There is a positive and weak relationship between age and ED visits

vars = c('B02001_002')
ethnicityData = get_acs(geography = "zcta", variables = vars, geometry = FALSE)


# Question 19: Rename the columns that need renaming, and clean up the dataframe
ethnicityData=rename(ethnicityData, White = estimate)
ethnicityData$mod_zcta = sub("ZCTA5 ", "", ethnicityData$NAME)
# glimpse(ethnicityData)

ethnicityData<-ethnicityData%>%select(-NAME)
ethnicityData<-ethnicityData%>%select(-variable)
ethnicityData<-ethnicityData%>%select(-GEOID)
# glimpse(ethnicityData)

# Question 20: Perform an Inner Join into the main data set

EthniandPopulation = inner_join(x=AgeandPopulation ,y = ethnicityData)
# glimpse(EthniandPopulation)

EthniandPopulation <- mutate(EthniandPopulation, 
                             NOfWhite = (White)/ (population))
# glimpse(EthniandPopulation)
EthniandPopulation<-EthniandPopulation%>%select(-moe)
EthniandPopulation<-EthniandPopulation%>%select(-NAME)

# glimpse(EthniandPopulation)
# Question 21: Analyze the Relationship between White Population and 
# Emergency Visits Per Inhabitant
EthniandPopulation <- mutate(EthniandPopulation, 
                             EMVPIH = (Number_of_Visits)/ (White))
glimpse(EthniandPopulation)
plot(EthniandPopulation$White, EthniandPopulation$EMVPIH, 
     xlab="White", ylab="EMVPIH")
mb<-lm(EthniandPopulation$EMVPIH~EthniandPopulation$White)
abline(mb,col="red")
summary(mb)$adj.r.squared
legend("topright",legend=paste("R-squared= ",
                               format(summary(mb)$adj.r.squared,digits=2)))

# glimpse(EthniandPopulation)

# We conclude that there is a negative and weak Relationship between White Population and Emergency Visits Per
# Inhabitant

# Multi-linear Regression
# Question 22: Perform a multi-linear regression of ED visits per inhabitant (for each ZIP)
# against 3 independant variables: "White Alone" percentage, median age and poverty rate

summary(lm(EthniandPopulation$rate ~EthniandPopulation$NOfWhite  + 
             EthniandPopulation$AgeNumber + EthniandPopulation$PovertyRate ))


# Question 23: Analyze the Strength of the Linear Relationship
# We have a dynamic dataset
# Increase the size of the dataset
# Go back and redo the same thing
library(ggplot2)