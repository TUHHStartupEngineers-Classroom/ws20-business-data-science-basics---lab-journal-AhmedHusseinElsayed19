library(scales)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(ggthemes)
library(dplyr)
library(maps)
library(ggmap)
library(maps)
library(mapdata)
library(devtools)

covid_data_tbl<- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

#Tables for Challenge 2 before plotting
covid_data_tbl%>%
  
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  ))

covid_world_mortality_rate_tbl<-covid_data_tbl%>%
  mutate(mortalityrate = (deaths/popData2019)*100) %>% 
  group_by(year,countriesAndTerritories) %>% 
  summarize(Mortality_Rates = sum(mortalityrate)) %>%
  ungroup()%>%
  select(countriesAndTerritories,Mortality_Rates,year)


#plotting without data
world <-   map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region) ,color = "black", fill = "lightgray", size = 0.1
  ) 

covid_world_mortality_rate_countries_tbl<-world%>%
  
  mutate(across(region, str_replace_all, "_", " ") )%>%
  mutate(region= case_when(
    
    region==  "UK"~"United_Kingdom" ,
    region == "USA"~"United_States_of_America" ,
    region == "Czechia"~"Czech_Republic",
    TRUE ~ region
  ))

covid_world_mortality_rate_countries_tbl<-covid_world_mortality_rate_countries_tbl %>%
  rename(countriesAndTerritories = region)

    #Combined table before plotting 
    covid_combined_world_mortality_rate <- left_join(covid_world_mortality_rate_countries_tbl,covid_world_mortality_rate_tbl, by = c("countriesAndTerritories"))

    ggplot(data = covid_combined_world_mortality_rate) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = Mortality_Rates), color= "white")
                  
  
 