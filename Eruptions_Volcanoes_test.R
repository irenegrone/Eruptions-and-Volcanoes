# -*- coding UTF-8 -*-

# title: "Eruptions and Volcanoes"
# author: "Irene Grone"
# date:  "December 2021"


# Import libraries ####

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(magrittr)
library(lubridate)
library(ggplot2)
library(flexdashboard)
library(tmap)
library(ggmap)


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


# missing start_month are replaced with month = 1
# missing VEI are replaced with rank unknown
eruption_data <- eruption_data %>%
  rename_at("evidence_method_(dating)", ~"evidence_method_dating") %>%
  mutate(
    vei = as.numeric(vei),
    start_year = as.numeric(start_year),
    start_month = as.numeric(start_month),
    evidence_method_dating = replace_na(evidence_method_dating, "unknown"),
    # assumption missing Explosivity Index are of rank 0
    vei = replace_na(vei, "unknown"),
    # missing `Start Month` are replaced with default 1
    start_month = replace_na(start_month, 1)
  ) %>%
  filter(!is.na(start_year)) %>%
  filter(volcano_name != "Unknown Source")


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
  ) %>%
  filter(volcano_name != "Unknown Source")





# Holocene ####
holocene <- read_xlsx(Holocene_FILE_PATH,
                                sheet = "Holocene Volcano List",
                                skip = 1)


names(holocene) <- str_to_lower(gsub(" ", "_", names(holocene)))
sapply(holocene, function(x) sum(is.na(x)))
summary(holocene)


holocene <- holocene %>%
  rename_at("elevation_(m)", ~"elevation") %>%
  select(!c(last_known_eruption, activity_evidence)) %>%
  mutate(
    dominant_rock_type = replace_na(dominant_rock_type, "Unknown"),
    tectonic_setting = replace_na(tectonic_setting, "Unknown"),
    primary_volcano_type = gsub("\\(es\\)", "", primary_volcano_type),
    primary_volcano_type = gsub("\\(s\\)", "", primary_volcano_type),
    primary_volcano_type = gsub("\\?", "", primary_volcano_type),
    primary_volcano_type = str_trim(primary_volcano_type),
  )








# Pleistocene ####
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
    tectonic_setting = replace_na(tectonic_setting, "Unknown"),
    primary_volcano_type = gsub("\\(es\\)", "", primary_volcano_type),
    primary_volcano_type = gsub("\\(s\\)", "", primary_volcano_type),
    primary_volcano_type = gsub("\\(\\?\\)", "", primary_volcano_type),
    primary_volcano_type = gsub("\\?", "", primary_volcano_type),
    primary_volcano_type = str_trim(primary_volcano_type),
  )





# Start Viz ####







# MAP ####
# prep data

holo_coord <- holocene %>%
  select(volcano_name, latitude, longitude,
         primary_volcano_type, tectonic_setting, elevation) %>%
  mutate(
    era = "Holocene"
  )


plei_coord <- pleistocene %>%
  select(volcano_name, latitude, longitude,
         primary_volcano_type, tectonic_setting, elevation) %>%
  mutate(
    era = "Pleistocene"
  )


volcanoes_geo <- holo_coord %>%
  union(plei_coord) %>%
  filter(volcano_name != "East Gakkel Ridge at 85°E")


volcanoes_geo %>%
  leaflet() %>%
  setView(lng = 0, lat = 0, zoom = 0) %>%
  addCircles(
    popup = ~paste0(volcanoes_geo$volcano_name, "<br />",
                    volcanoes_geo$primary_volcano_type, "<br />",
                    volcanoes_geo$tectonic_setting, "<br />",
                    volcanoes_geo$elevation, "<br />")
  )

# test 1 ####
w_map <- geom_point(aes(x = longitude, y = latitude, colour = era),
             data = volcanoes_geo,
             alpha=0.5,
             size = 1) +
  theme_bw() +
  labs("Era",
       x = "Longitude", y = "Latitude",
       colour = "era",
  ) +
  scale_fill_manual("Volcanoes in the world",
                    values=c("darkblue","brown")) +
  theme(legend.position="bottom")







# BAR PLOT ####

holocene %>%
  group_by(primary_volcano_type) %>%
  mutate(
    cnt = n()
  ) %>%
  ggplot(aes(primary_volcano_type, cnt)) +
  geom_bar(aes(fill=primary_volcano_type), stat = "identity") +
  geom_text(aes(label = cnt),
            position = position_dodge(width = 0.8),
            vjust= -0.25) +
  theme_minimal() +
  labs(
    title = "Primary volcano type",
    caption = "",
    x = "",
    y = "",
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
        legend.position = "none")



