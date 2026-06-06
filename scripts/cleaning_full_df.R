#########################
# Purpose: Reducing the full dataframe 
# Last updated: 05.06.2026
# Author: Pia Alina Brakstad Smit
#########################

library(tidyverse)
library(lubridate)

load(file = "data/full_dataframe.rda")

# The reduced dataframe will merge cols that are inconsistent across dataframes
reduced_df <- full_df |> 
  # Species
  mutate(Species = coalesce(commonname, SpeciesCategory), 
         Species = recode(Species,
                          "hyse" = "Haddock",
                          "torsk" = "Cod")) |> 
  filter(!Species == "sei") |> #removing saithe
  # Individual
  mutate(Individual = coalesce(specimenid, IndividualKey)) |> 
  # Latitude and Longitude
  mutate(Latitude = coalesce(latitudestart, Latitude), #OBS data har NAs
         Longitude = coalesce(longitudestart, Longitude)) |> 
  # DateTimeStation, Year, and DayNumber
  mutate(DateTimeStation = coalesce(ymd_hms(DateTime), ymd(stationstartdate)),
         Year = year(DateTime),
         DayNumber = yday(DateTimeStation)) |> 
  # Age
  mutate(Age = coalesce(age, IndividualAge)) |> 
  # Sex 
  mutate(Sex = coalesce((as.character(sex)), IndividualSex),
         Sex = recode(Sex, 
                      "1" = "F",
                      "2" = "M")) |> 
  # Weight 
  mutate(Weight = coalesce((individualweight*1000), IndividualRoundWeight)) |> 
  # Length 
  mutate(Length = as.numeric(length*100)) |> 
  # MissionTypeName
  #consider making Survey into Forskningsfartøy
  mutate(MissionTypeName = coalesce(missiontypename, Survey)) |> 
  # Gonadweight 
  mutate(Gonadweight = gonadweight*1000) |> 
  # Liverweight 
  mutate(Liverweight = liverweight*1000) |> 
  # GSI
  mutate(GSI = (gonadweight/Weight)*100) |> 
  select(
    dataID, Year, DateTimeStation, DayNumber, Latitude, Longitude, MissionTypeName, 
    Species, Individual, Length, Weight, Maturationstage = maturationstage,
    Gonadweight, Liverweight,
         Sex, Age, GSI)


test1 <- reduced_df |> 
  group_by(Sex) |> 
  summarise(
    length = mean(length, na.rm = TRUE),
    Weight = mean(Weight, na.rm = TRUE)
  )
#coalesce(as.character(startyear),

test2 <- data |> 
  group_by(sex) |> 
  summarise(
    length = mean(length, na.rm = TRUE),
    Weight = mean(individualweight, na.rm = TRUE),
    minlength = min(length, na.rm = TRUE),
    minWeight = min(individualweight, na.rm = TRUE),
    meangonad = mean(gonadweight, na.rm = TRUE),
    meanliver = mean(liverweight, na.rm = TRUE),
    mingonad = min(gonadweight, na.rm = TRUE),
    minliver = min(liverweight, na.rm = TRUE),
    maxgonad = max(gonadweight, na.rm = TRUE),
    maxliver = max(liverweight, na.rm = TRUE)
  )

test3 <- torskøko |> 
  group_by(IndividualSex) |> 
  summarise(
    length = mean(length, na.rm = TRUE),
    Weight = mean(IndividualRoundWeight, na.rm = TRUE),
    minlength = min(length, na.rm = TRUE),
    minWeight = min(IndividualRoundWeight, na.rm = TRUE),
    meangonad = mean(gonadweight, na.rm = TRUE),
    meanliver = mean(liverweight, na.rm = TRUE),
    mingonad = min(gonadweight, na.rm = TRUE),
    minliver = min(liverweight, na.rm = TRUE),
    maxgonad = max(gonadweight, na.rm = TRUE),
    maxliver = max(liverweight, na.rm = TRUE)
  )
data_plot <- ggplot(data, aes(
  y = length,
  x = individualweight
)) +
  geom_point()

data_plotgonad <- ggplot(data, aes(
  x = gonadweight
)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(limits = c(1,6))

print("testingtesting")
