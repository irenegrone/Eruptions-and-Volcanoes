# title: "Eruptions Volcanoes analysis"
# author: "Irene Grone"
# date: "09/06/2021"
#

# Import libraries ####

library(tidyverse)
library(readxl)
library(lubridate)



# Load data ####
Eruption_FILE_PATH = paste0(getwd() ,"/data/GVP_Eruption_Results.xlsx")
Holocene_FILE_PATH = paste0(getwd() ,"/data/GVP_Volcano_List_Holocene.xlsx")
Pleistocene_FILE_PATH = paste0(getwd() ,"/data/GVP_Volcano_List_Pleistocene.xlsx")

eruption_data <- read_xlsx(Eruption_FILE_PATH, sheet = "Eruption List", skip = 1)

eruption_events_data <- read_xlsx(Eruption_FILE_PATH, sheet = "Events", skip = 1)

