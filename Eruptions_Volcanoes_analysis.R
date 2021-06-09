# title: "Eruptions Volcanoes analysis"
# author: "Irene Grone"
# date: "09/06/2021"
#

# Import libraries ####

library(tidyverse)
library(readxl)
library(lubridate)



# Load data ####

# change working directory as appropriate
# setwd('C:/Users/irene/Documents/GitHub/Eruptions-and-Volcanoes')

Eruption_FILE_PATH = paste0(getwd() ,"/data/GVP_Eruption_Result.xlsx")

Holocene_FILE_PATH = "data/GVP_Volcano_List_Holocene.xlsx"

Pleistocene_FILE_PATH = "data/GVP_Volcano_List_Pleistocene.xlsx"


eruption_data <- read_xlsx(Eruption_FILE_PATH, sheet = "transport data", skip = 1)

