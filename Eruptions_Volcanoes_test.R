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
library(ggplot2)
library(flexdashboard)
library(leaflet)
library(plotly)
library(treemapify)


# Load  and clean data ####

Eruption_FILE_PATH = "data/GVP_Eruption_Results.xlsx"
Holocene_FILE_PATH = "data/GVP_Volcano_List_Holocene.xlsx"
Pleistocene_FILE_PATH = "data/GVP_Volcano_List_Pleistocene.xlsx"


# Holocene ####
holocene <- read_xlsx(Holocene_FILE_PATH,
                                sheet = "Holocene Volcano List",
                                skip = 1)


names(holocene) <- str_to_lower(gsub(" ", "_", names(holocene)))


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
  ) %>%
  select(volcano_number, volcano_name, latitude, longitude,
         region, country, primary_volcano_type, tectonic_setting,
         elevation, dominant_rock_type) %>%
  mutate(
    era = "Holocene"
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


pleistocene <- pleistocene %>%
  rename_at("elevation_(m)", ~"elevation") %>%
  mutate(
    tectonic_setting = replace_na(tectonic_setting, "Unknown"),
    primary_volcano_type = gsub("\\(es\\)", "", primary_volcano_type),
    primary_volcano_type = gsub("\\(s\\)", "", primary_volcano_type),
    primary_volcano_type = gsub("\\(\\?\\)", "", primary_volcano_type),
    primary_volcano_type = gsub("\\?", "", primary_volcano_type),
    primary_volcano_type = str_trim(primary_volcano_type),
    dominant_rock_type = "-"
  ) %>%
  select(volcano_number, volcano_name, latitude, longitude,
         region, country, primary_volcano_type, tectonic_setting,
         elevation, dominant_rock_type) %>%
  mutate(
    era = "Pleistocene",
  )


# volcanoes unified dataset
volcanoes_geo <- holocene %>%
  union(pleistocene) %>%
  separate(tectonic_setting, c("tectonic_zone", "tectonic_crust"), " / ") %>%
  mutate(
    tectonic_crust = gsub("crust ", "", tectonic_crust)
  ) %>%
  mutate(
    primary_volcano_type = as.factor(primary_volcano_type),
    dominant_rock_type = as.factor(dominant_rock_type),
    tectonic_zone = as.factor(tectonic_zone),
    tectonic_crust = as.factor(tectonic_crust),
    era = as.factor(era)
  )


# MAP ####

leaflet(volcanoes_geo,  padding = 10) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  setView(lng = -0, lat = 0, zoom = 1.5) %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   radius = 1,
                   color = ifelse(volcanoes_geo$era == "Holocene",
                                  "darkgreen", "royalblue"),

                  ) %>%
  addLegend("bottomright",
            colors = c("darkgreen", "royalblue"),
            labels = c("Holocene", "Pleistocene"),
            title = "Era"
            )


# Volcano type ####

# Primary volcano type
p <- volcanoes_geo %>%
  count(era, primary_volcano_type, .drop=FALSE) %>%
  ggplot(aes(fill = era,
             x = primary_volcano_type,
             y = n)
  ) +
  geom_bar(position="dodge",
           stat="identity"
  ) +
  scale_fill_manual("Era", values=c("#8AD4C0", "royalblue")) +
  labs(
    x = "Volcano type",
    y = "Count",
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
        legend.position = c(.90, .90),
  )

ggplotly(p)


# By region ####

region <- volcanoes_geo %>%
  count(region, .drop=FALSE) %>%
  ggplot(aes(fill = "#FFB46E",
             x = region,
             y = n)
  ) +
  geom_text(aes(label=n),
            color = "black",
            size = 3.5
  ) +
  geom_bar(position="dodge",
           stat="identity"
  ) +
  scale_fill_manual(values=c("#FFB46E")) +
  labs(
    x = "Region",
    y = "Count",
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()


ggplotly(region) %>% layout(hovermode=F)


# Elevation distribution ####

elev_density <- volcanoes_geo %>%
  ggplot(aes(elevation,
             fill = era
             )
  ) +
  geom_density(alpha = 0.3,
               linetype = "none"
               ) +
  scale_fill_manual("Era", values=c("#8AD4C0", "royalblue")) +
  theme_minimal()


ggplotly(elev_density)


# Tectonic setting ####

tect_col <- c("#701510", "lightgrey", "#17732F", "#94DAFF")

tect_density <- volcanoes_geo %>%
  filter(tectonic_zone != "Unknown") %>%
  ggplot(aes(elevation,
             fill = tectonic_crust,
             )
  ) +
  geom_density(alpha = 0.2) +
  facet_grid(vars(era), vars(tectonic_zone)) +
  scale_fill_manual("Crust type", values=tect_col) +
  theme_minimal()


ggplotly(tect_density)


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


eruption_data <- eruption_data %>%
  rename_at("evidence_method_(dating)", ~"evidence_method_dating") %>%
  filter(!is.na(start_year)) %>%
  filter(volcano_name != "Unknown Source") %>%
  filter(eruption_number != 21100) %>%
  mutate(
    vei = as.numeric(vei),
    start_year = as.numeric(start_year),
    start_month = as.numeric(start_month),
    evidence_method_dating = replace_na(evidence_method_dating, "unknown"),
    # missing `Start Month` are replaced with default 1
    start_month = replace_na(start_month, 1)
  ) %>%
  left_join(volcanoes_geo[, c("volcano_number",
                              "tectonic_zone")],
            by="volcano_number",
            copy = FALSE)


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


# count in time ####

erup_time <- eruption_data %>%
  count(start_year, .drop=FALSE) %>%
  ggplot(aes(
             x = start_year,
             y = n)
  ) +
  geom_line(colour = "#FFB46E",
            size=0.3
  ) +
  labs(
    x = "Start year",
    y = "Count",
  ) +
  xlim(0, 2020) +
  theme_minimal() +
  theme(legend.position = "none")


ggplotly(erup_time) %>% layout(hovermode=F)


# VEI distribution by Tectonic zone  ####

vei_zone <- eruption_data %>%
  filter(tectonic_zone != "Unknown") %>%
  filter(vei >= 0) %>%
  ggplot(aes(vei,
             fill = tectonic_zone,
             )
         ) +
  geom_bar(alpha = 0.5,
               linetype = "none"
               ) +
  facet_grid(cols=vars(tectonic_zone)) +
  scale_fill_manual("Tectonic zone",
                    values = c("#701510", "#17732F", "#94DAFF")) +
  theme_minimal()


ggplotly(vei_zone) %>% layout(hovermode=F)


# DATING METHODS ####

dating_met <- eruption_data %>%
  count(evidence_method_dating, .drop=FALSE) %>%
  ggplot(aes(fill = n,
             area = n,
             label = paste0(evidence_method_dating,
                            "\n",
                            prettyNum(n)))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 10,
                    reflow = T) +
  theme_minimal()

dating_met

