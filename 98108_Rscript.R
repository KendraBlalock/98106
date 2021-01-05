#Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

##Pet licenses
#Data from https://catalog.data.gov/dataset/seattle-pet-licenses
pet <- read.csv("Seattle_Pet_Licenses.csv")

#Filter for 98108 zip code and reformat year
pet <- pet %>% 
  filter(ZIP.Code == 98108) %>% 
  mutate(License.Date = as.Date(License.Issue.Date, format = "%B %d %Y")) %>% 
  mutate(License.Date = format(License.Date, "%Y")) 

#Cat vs. Dog in past 2 years
pet %>% 
  filter(License.Date == "2019" | License.Date == "2020") %>% 
  count(Species)

#Top Dog Breeds
pet %>% filter(License.Date == "2019" | License.Date == "2020") %>% 
  filter(Species != "Cat") %>% 
  count(Primary.Breed) %>% 
  arrange(desc(n)) %>% 
  head()

#Top names of past 2 years
pet %>% filter(License.Date == "2019" | License.Date == "2020") %>% 
  count(Animal.s.Name) %>% 
  arrange(desc(n)) %>% 
  head()

pet %>% filter(License.Date == "2019" | License.Date == "2020") %>% 
  count(Species, Animal.s.Name) %>% 
  arrange(desc(n)) %>% 
  head()


rm(pet)

##Resturant ratings
#Data from https://data.kingcounty.gov/Health/Food-Establishment-Inspection-Data/4umv-2xev
food <- read.csv("Food_Establishment_Inspection_Data.csv")

#Filter for 98108 zip code and reformat year
food <- food %>% 
  filter(Zip.Code == 98108) %>% 
  mutate(Inspection.Year = as.Date(Inspection.Date, format = "%m/%d/%Y")) %>% 
  mutate(Inspection.Year = format(Inspection.Year, "%Y")) 

#Growth of restaurants from 2010 to 2019
food %>% count(Inspection.Year)

food %>% select(Inspection.Year, Program.Identifier) %>% 
  count(Inspection.Year, Program.Identifier) %>% 
  select(-n) %>% 
  count(Inspection.Year) %>% 
  filter(Inspection.Year == 2009 | Inspection.Year == 2019)

#Average restaurant score
food %>% filter(Inspection.Year == "2019") %>% 
  count(Inspection.Result) %>% 
  filter(Inspection.Result != "Complete")

rm(food)

##County sheriff
#Data from https://data.kingcounty.gov/Law-Enforcement-Safety/King-County-Sheriff-s-Office-Incident-Dataset/rzfs-wyvy
sheriff <- read.csv("King_County_Sheriff_s_Office_-_Incident_Dataset.csv")

#Filter for 98108
sheriff <- sheriff %>% filter(zip == "98108")

#Incident type
sheriff %>% count(incident_type) %>%
  arrange(desc(n)) %>% 
  filter(incident_type != "Other") %>% 
  head()

sheriff %>% filter(incident_type != "Other") %>% 
  count(incident_type, day_of_week) %>%
  arrange(desc(n)) %>% 
  head(1)

rm(sheriff)

## Cultural Spaces
#Data from https://data.seattle.gov/Community/Seattle-Cultural-Space-Inventory/vsxr-aydq
Cultural <- read.csv("Seattle_Cultural_Space_Inventory.csv")

#Filter for address containing 98108
Cultural %>% mutate(zip = case_when(
  str_detect(Address, "98108") ~ 1,
  T ~ 0
)) %>% 
  filter(zip == 1) %>% 
  count()

rm(Cultural)

##US Census data for 98108
#Data from https://data.census.gov/cedsci/
#2019 estimates

#Population
pop <- read.csv("Census Data/ACSDP5Y2019.DP05_data_with_overlays_2020-12-12T211509.csv")

#Total population
pop %>% select(DP05_0001E)

#% female
pop %>% select(DP05_0003PE)

#Race breakdown
pop %>% select(DP05_0037PE, DP05_0038PE, DP05_0039PE, DP05_0044PE, 
               DP05_0052PE, DP05_0057PE, DP05_0058PE)

#% Latinx
pop %>% select(DP05_0071PE)

rm(pop)

##Employment industry
employ <- read.csv("Census Data/ACSST5Y2019.S0501_data_with_overlays_2020-12-12T211509.csv")

#Most common industry 
employ %>% select(S0501_C01_075E)

#Median income
employ %>% select(S0501_C01_101E)

#Has a car 
employ %>% select(S0501_C01_129E)

#Owner occupied houses
employ %>% select(S0501_C01_117E)
