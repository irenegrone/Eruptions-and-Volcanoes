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
eruption_events_data <- read_xlsx(Eruption_FILE_PATH,
                                  sheet = "Events",
                                  skip = 1)[, c("Volcano Number",
                                                "Volcano Name",
                                                "Eruption Number",
                                                "Eruption Start Year",
                                                "Event Number",
                                                "Event Type")]


sapply(eruption_events_data, function(x) sum(is.na(x)))




# Volcano list Holocene ####
holocene_volcanoes <- read_xlsx(Holocene_FILE_PATH,
                                sheet = "Holocene Volcano List",
                                skip = 1)


sapply(holocene_volcanoes, function(x) sum(is.na(x)))


# Volcano list Pleistocene ####
pleistocene_volcanoes <- read_xlsx(Pleistocene_FILE_PATH,
                                   sheet = "Pleistocene Volcano List",
                                   skip = 1)


sapply(pleistocene_volcanoes, function(x) sum(is.na(x)))

pleistocene_volcanoes <- pleistocene_volcanoes %>%
  select(-c('Activity Evidence',
            'Last Known Eruption',
            'Dominant Rock Type'))

