## Data wrangling for off-campus data from CSV files
## Written by Gillian McGinnis
## Created 20 October 2020
## Updated 20 October 2020
## ## edits 21 october 2020 - kbott

library(tidyverse) ## Needed for dplyr, stringr, etc.
library(lubridate) ## Needed for time conversions since our data is in milliseconds.
library(janitor) ## clean up capitalization

# read in initial data
dataClinton <- read_csv("data/dataClinton.csv")
dataWoodstock <- read_csv("data/dataWoodstock.csv")
dataTimes <- read_csv("data/dataTimes.csv")

## ## data cleaning to-dos:
## remove example data
## add location
## tidy, generally
## add time data

## ## starting w dataClinton
dataClinton <- dataClinton %>%
  mutate(location = "Clinton") %>%
  ## use regex + pivot_longer to create variables sensor / measurement / site.
  pivot_longer(
    !location,
    names_to = c("sensor", "measurement", "site"),
       names_pattern = "S(.{2})(.*)_(.*)",
               values_to = ("value")) %>%
  filter(!is.na(value))

## ## repeat for dataWoodstock
dataWoodstock <- dataWoodstock %>%
  mutate(location = "Woodstock") %>%
  ## use regex + pivot_longer to create variables sensor / measurement / site.
  pivot_longer(
    !location,
    names_to = c("sensor", "measurement", "site"),
    names_pattern = "S(.{2})(.*)_(.*)",
    values_to = ("value")) %>%
  filter(!is.na(value))

## ## combine dataframes + reorder
all_data <- bind_rows(dataWoodstock, dataClinton) %>%
  select("location", "site", "sensor", "measurement", "value")

## location info - need to set consistently -- first, double-check values for both datasets
### sensor data
all_data %>%
  select(site) %>%
  group_by(site) %>%
  count()

### time data
dataTimes %>%
  select(sublocation) %>%
  group_by(sublocation) %>%
  count()

## ## cleaning up time data
data_times <- dataTimes %>% 
  # all variables to snake_case
  clean_names() %>%
  # all values of `sublocation` to lowercase
  mutate(sublocation = str_to_lower(sublocation)) %>%
  # consistency for firehydrant / library sites
  mutate(site = case_when(sublocation == "firehydrant" ~ "fire",
                          sublocation == "front of library" ~ "lib")) %>%
  # populate missing values w/ the sublocation data
  mutate(site = if_else(!is.na(site), site, sublocation)) %>%
  # keep sensor data consistent w other sheet
  mutate(sensor = str_sub(sensor, 2, 3)) %>%
  # retain what's needed, reorder
  select(location, site, sensor, start, end)

## ## combine dataframes
all_data_times <- left_join(all_data, data_times)

## ## not completely done.

## ideas::
## fix time data so the seconds elapsed ("time" measurement) are added to the interval properly
## if you need that?
## possibly widen time data, up to gillian