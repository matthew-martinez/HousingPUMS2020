library(tidyverse)
library(data.table)
library(srvyr)
library(reactable)
library(tidycensus)

statePops <- get_estimates(geography = "state", product = "housing", output="wide", year=2019)

data2 <- fread("C:\\Users\\mmartinez\\Documents\\Data\\PUMS 2020\\csv_hus\\psam_husa.csv") # a lot faster, same memory usage

data3 <- fread("C:\\Users\\mmartinez\\Documents\\Data\\PUMS 2020\\csv_hus\\psam_husb.csv") # a lot faster, same memory usage

data4 <- rbind(data2, data3) # combine both sets of pums data - 1.3gb total

rm(data2)
rm(data3)

gc() # clears memory even after rm()

#data4 <- data4[,c("RT","SERIALNO","ST","NP","TYPEHUGQ","WGTP","TEN","GRPIP", "OCPIP")] 

gc() 

replist <- rep(paste0("WGTP",1:80)) # creating list of weights

## Data check ##

check <- data4 %>%
  as_survey_rep(
    weight=WGTP,
    repweights = replist,
    scale = 4/80,
    rscales = rep(1,80),
    mse=TRUE,
    type="other",
    variables=c(ST, TEN, TYPEHUGQ)
  )

check %>%
  group_by(ST, TEN) %>%
  summarise(TEN = survey_total(vartype=c("se", "cv"))) %>%
  mutate(MOE = TEN_se * 1.645)

# TYPEHUGQ Character 1 
# Type of unit 
# 1 .Housing unit 
# 2 .Institutional group quarters 
# 3 .Noninstitutional group quarters

# TEN Character 1 
# Tenure 
# b .N/A (GQ/vacant) 
# 1 .Owned with mortgage or loan (include home equity loans) 
# 2 .Owned free and clear 
# 3 .Rented 
# 4 .Occupied without payment of rent 

# GRPIP Numeric 3 
# Gross rent as a percentage of household income past 12 months 
# bbb .N/A (GQ/vacant/owned or being bought/occupied withoutrent payment/no household income) 
# 1..100 .1 percent to 100 percent 
# 101 .101 percent or more 

# OCPIP Numeric 3 
# Selected monthly owner costs as a percentage of household income during 
# the past 12 months 
# bbb .N/A (GQ/vacant/not owned or being bought/ no household income) 
# 1..100 .1 percent to 100 percent 
# 101 .101 percent or more

data4 <- data4 %>%
  #filter( # not vacant
  #       TEN == 1 | TEN == 3) %>% # tenure types owned w/ mortgage or loan and rented
  select(RT, ST, TEN, GRPIP, OCPIP, NP, TYPEHUGQ, WGTP, replist)

data4 <- data4 %>%
  mutate(rentBurden = ifelse(GRPIP >= 30, "Burdened", "Not Burdened"),
         ownBurden = ifelse(OCPIP >= 30, "Burdened", "Not Burdened"),
         severeRentBurden = ifelse(GRPIP >= 50, "Severely Burdened", "Not Severely Burdened"),
         severeOwnBurden = ifelse(OCPIP >= 50, "Severely Burdened", "Not Severely Burdened"),
         ST = str_pad(ST, 2, "left", 0))

data4 <- merge(statePops, data4, by.x="GEOID", by.y="ST")

# householdSurvey <- data4 %>%
#   as_survey_rep(
#     weight=WGTP,
#     repweights = replist,
#     scale = 4/80,
#     rscales = rep(1,80),
#     mse=TRUE,
#     type="other",
#     variables=c(GEOID, NAME, TEN, rentBurden, ownBurden, severeRentBurden, severeOwnBurden)
#   )

householdSurvey <- data4 %>%
  as_survey_design(
    weight=WGTP1
  )

## State

RentBurden <- householdSurvey %>%
  filter(TEN == 3) %>%
  filter(is.na(rentBurden) == F) %>%
  group_by(NAME, rentBurden) %>%
  summarise(Rent = survey_mean(vartype=c("se", "cv"))) %>%
  mutate(MOE = Rent_se * 1.645) %>%
  filter(rentBurden == "Burdened")

OwnBurden <- householdSurvey %>%
  filter(TEN == 1) %>%
  filter(is.na(ownBurden) == F) %>%
  group_by(NAME, ownBurden) %>%
  summarise(Own = survey_mean(vartype=c("se", "cv"))) %>%
  mutate(MOE = Own_se * 1.645) %>%
  filter(ownBurden == "Burdened")

SevereRentBurden <- householdSurvey %>%
  filter(TEN == 3) %>%
  filter(is.na(severeRentBurden) == F) %>%
  group_by(NAME, severeRentBurden) %>%
  summarise(SevereRent = survey_mean(vartype=c("se", "cv"))) %>%
  mutate(MOE = SevereRent_se * 1.645) %>%
  filter(severeRentBurden == "Severely Burdened")

SevereOwnBurden <- householdSurvey %>%
  filter(TEN == 1) %>%
  filter(is.na(severeOwnBurden) == F) %>%
  group_by(NAME, severeOwnBurden) %>%
  summarise(SevereOwn = survey_mean(vartype=c("se", "cv"))) %>%
  mutate(MOE = SevereOwn_se * 1.645) %>%
 filter(severeOwnBurden == "Severely Burdened")

## USA
RentBurdenState <- householdSurvey %>%
  filter(TEN == 3) %>%
  filter(is.na(rentBurden) == F) %>%
  group_by(rentBurden) %>%
  summarise(Rent = survey_mean(vartype=c("se", "cv"))) %>%
  mutate(MOE = Rent_se * 1.645,
         NAME = "United States") %>%
  filter(rentBurden == "Burdened")

OwnBurdenState <- householdSurvey %>%
  filter(TEN == 1) %>%
  filter(is.na(ownBurden) == F) %>%
  group_by(ownBurden) %>%
  summarise(Own = survey_mean(vartype=c("se", "cv"))) %>%
  mutate(MOE = Own_se * 1.645,
         NAME = "United States") %>%
  filter(ownBurden == "Burdened")

SevereRentBurdenState <- householdSurvey %>%
  filter(TEN == 3) %>%
  filter(is.na(severeRentBurden) == F) %>%
  group_by(severeRentBurden) %>%
  summarise(SevereRent = survey_mean(vartype=c("se", "cv"))) %>%
  mutate(MOE = SevereRent_se * 1.645,
         NAME = "United States") %>%
  filter(severeRentBurden == "Severely Burdened")

SevereOwnBurdenState <- householdSurvey %>%
  filter(TEN == 1) %>%
  filter(is.na(severeOwnBurden) == F) %>%
  group_by(severeOwnBurden) %>%
  summarise(SevereOwn = survey_mean(vartype=c("se", "cv"))) %>%
  mutate(MOE = SevereOwn_se * 1.645,
         NAME = "United States") %>%
  filter(severeOwnBurden == "Severely Burdened")

fullOwn <- rbind(OwnBurden,OwnBurdenState)
fullRent <- rbind(RentBurden,RentBurdenState)

fullDf <- merge(fullOwn, fullRent, by="NAME")

fullDf <- fullDf %>%
  mutate(rentBurden = round(Rent*100,1),
         ownBurden = round(Own*100,1)) %>%
  select(NAME, rentBurden, ownBurden) %>%
  rename(State = NAME)

write.csv(fullDf, "\\\\SERVER-3\\Public\\Matthew\\housingcosts\\newline\\PUMS2020.csv", row.names=FALSE)
