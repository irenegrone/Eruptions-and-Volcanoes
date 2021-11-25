# -*- coding UTF-8 -*-

# title: "Eruptions  and Volcanoes"
# author: "Irene Grone"
# date: "09/06/2021"


# Import libraries ####

library(tidyverse)
library(readxl)
library(lubridate)
library(flexdashboard)


# Load data ####

Eruption_FILE_PATH = paste0(getwd() ,"/data/GVP_Eruption_Results.xlsx")

Holocene_FILE_PATH = paste0(getwd() ,"/data/GVP_Volcano_List_Holocene.xlsx")

Pleistocene_FILE_PATH = paste0(getwd() ,"/data/GVP_Volcano_List_Pleistocene.xlsx")

# Eruptions
eruption_data <- read_xlsx(Eruption_FILE_PATH,
                           sheet = "Eruption List", skip = 1)
eruption_events_data <- read_xlsx(Eruption_FILE_PATH,
                                  sheet = "Events", skip = 1)

# Volcano list Holocene
holocene_volcanoes <- read_xlsx(Holocene_FILE_PATH,
                                sheet = "Holocene Volcano List", skip = 1)

# Volcano list Pleistocene
pleistocene_volcanoes <- read_xlsx(Pleistocene_FILE_PATH,
                                   sheet = "Pleistocene Volcano List", skip = 1)


# EDA ####

# check NAs

sapply(eruption_data, function(x) sum(is.na(x)))

sapply(eruption_events_data, function(x) sum(is.na(x)))

sapply(holocene_volcanoes, function(x) sum(is.na(x)))

sapply(pleistocene_volcanoes, function(x) sum(is.na(x)))
