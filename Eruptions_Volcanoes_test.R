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
library(rworldmap)
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


# Elevation ####
pleistocene %>%
  mutate(
    bin_elevation = cut_width(elevation, width = 1000, center = 500)
  ) %>%
  select(volcano_name, bin_elevation) %>%
  ggplot() +
  geom_bar(aes(bin_elevation)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
        legend.position = "none")




barplot(height = pleistocene$elevation,
         # names="none",
        col = ifelse(pleistocene$elevation > 0, 2, 4),
        border = NA
        )



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


# test 1 ####
worldmap <- getMap(resolution = "coarse")

plot(worldmap,
     col = "lightgray",
     fill = T,
     border = "darkgray",
     xlim = c(-180, 180),
     ylim = c(-90, 90),
     bg = "lightblue",
     asp = 1,
     wrap=c(-180, 180)
     )

points(volcanoes_geo$longitude,
       volcanoes_geo$latitude,
       col = ifelse(volcanoes_geo$era == "Holocene", "darkorange", "darkgreen"),
       cex = 0.3
       )

# test 2 ####
library(rnaturalearth)
library(sp)

map <- ne_countries()
names(map)[names(map) == "iso_a3"] <- "ISO3"
names(map)[names(map) == "name"] <- "NAME"


plot(map,
     col = "lightgray",
     fill = T,
     border = "darkgray",
     xlim = c(-180, 180),
     ylim = c(-90, 90),
     bg = "lightblue",
     asp = 1,
     wrap=c(-180, 180)
)

points(volcanoes_geo$longitude,
       volcanoes_geo$latitude,
       col = ifelse(volcanoes_geo$era == "Holocene", "darkorange", "darkgreen"),
       cex = 0.3)


# test 3 ####
library(leaflet)

leaflet(volcanoes_geo,  padding = 10) %>%
  addTiles() %>%
  setView(lng = -0, lat = 0, zoom = 1.5) %>%
  addCircleMarkers(lng = ~longitude,
                  lat = ~latitude,
                  radius = 1,
                  color = ifelse(volcanoes_geo$era == "Holocene",
                                 "darkorange", "royalblue"),

                  )



#  ) %>%
  leaflet::addLegend(
    pal = pal, values = ,
    opacity = 0.7, title = "PM2.5"
  )





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



