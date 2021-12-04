# -*- coding UTF-8 -*-

# title: "Eruptions  and Volcanoes"
# author: "Irene Grone"
# date:  "December 2021"


# Import libraries ####

library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(magrittr)
library(lubridate)
library(ggplot2)
library(flexdashboard)


# Load  and clean data ####

Eruption_FILE_PATH = "data/GVP_Eruption_Results.xlsx"
Holocene_FILE_PATH = "data/GVP_Volcano_List_Holocene.xlsx"
Pleistocene_FILE_PATH = "data/GVP_Volcano_List_Pleistocene.xlsx"


# Eruptions ####
eruption_data <- read_xlsx(Eruption_FILE_PATH,
                           sheet = "Eruption List",
                           skip = 1)[, c("Volcano Number",
                                         "Volcano Name",
                                         "Eruption Number",
                                         "VEI",
                                         "Start Year",
                                         "Start Month",
                                         "Evidence Method (dating)",
                                         "Latitude",
                                         "Longitude")]

names(eruption_data) <- str_to_lower(gsub(" ", "_", names(eruption_data)))


summary(eruption_data)
sapply(eruption_data, function(x) sum(is.na(x)))


eruption_data %>%
  filter(is.na(vei)) %>%
  select(start_year) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(50)


eruption_data %>%
  filter(is.na(start_month)) %>%
  select(start_year) %>%
  table() %>%
  sort(decreasing = TRUE) %>% head(50)


eruption_data <- eruption_data %>%
  rename_at("evidence_method_(dating)", ~"evidence_method_dating") %>%
  mutate(
    vei = as.numeric(vei),
    start_year = as.numeric(start_year),
    start_month = as.numeric(start_month),
    evidence_method_dating = replace_na(evidence_method_dating, "unknown"),
    # assumption missing Explosivity Index are of rank 0
    vei = replace_na(vei, 0),
    # all missing `Start Month` are BC, replace with default 1
    start_month = replace_na(start_month, 1)
  )


# Eruption events ####
events_data <- read_xlsx(Eruption_FILE_PATH,
                         sheet = "Events",
                         skip = 1)[, c("Volcano Number",
                                       "Volcano Name",
                                       "Eruption Number",
                                       "Eruption Start Year",
                                       "Event Number",
                                       "Event Type")]


names(events_data) <- str_to_lower(gsub(" ", "_", names(events_data)))
summary(events_data)
sapply(events_data, function(x) sum(is.na(x)))

events_data <- events_data %>%
  mutate(
    eruption_start_year = as.numeric(eruption_start_year)
  )


# Volcano list Holocene ####
holocene <- read_xlsx(Holocene_FILE_PATH,
                      sheet = "Holocene Volcano List",
                      skip = 1)


names(holocene) <- str_to_lower(gsub(" ", "_", names(holocene)))
sapply(holocene, function(x) sum(is.na(x)))
summary(holocene)


holocene <- holocene %>%
  rename_at("elevation_(m)", ~"elevation") %>%
  mutate(
    dominant_rock_type = replace_na(dominant_rock_type, "unknown"),
    tectonic_setting = replace_na(tectonic_setting, "unknown")
    # HERE ####
    # do something with last_known_eruption
  )


# Volcano list Pleistocene ####
pleistocene <- read_xlsx(Pleistocene_FILE_PATH,
                         sheet = "Pleistocene Volcano List",
                         skip = 1)[ , c("Volcano Number",
                                        "Volcano Name",
                                        "Country",
                                        "Primary Volcano Type",
                                        "Region",
                                        "Subregion",
                                        "Latitude",
                                        "Longitude",
                                        "Elevation (m)",
                                        "Tectonic Setting")]


names(pleistocene) <- str_to_lower(gsub(" ", "_", names(pleistocene)))
sapply(pleistocene, function(x) sum(is.na(x)))
summary(pleistocene)

pleistocene <- pleistocene %>%
  rename_at("elevation_(m)", ~"elevation") %>%
  mutate(
    tectonic_setting = replace_na(tectonic_setting, "unknown")
  )


# Start Viz ####

