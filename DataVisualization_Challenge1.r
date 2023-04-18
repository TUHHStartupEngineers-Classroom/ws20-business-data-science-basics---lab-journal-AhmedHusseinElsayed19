library(scales)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(ggthemes)
library(dplyr)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

#Table for Challenge 1 before plot

  covid_data_select_tbl<- covid_data_tbl %>%
  select(countriesAndTerritories,cases,dateRep,month,year,day)%>%
  relocate(year,month,day)%>%
  filter(year==2020,month>1) %>%
  filter(day!=1)%>%
  filter(countriesAndTerritories=="France"|countriesAndTerritories=="Germany"|countriesAndTerritories=="United_Kingdom"|countriesAndTerritories=="Spain"|countriesAndTerritories=="United_States_of_America")%>%
  group_by(countriesAndTerritories,month)%>%
  summarize(totalcases = sum(cases)) %>%
  ungroup()
    

covid_data_select_tbl%>%

  
  ggplot(aes(month ,totalcases, color = countriesAndTerritories)) +
    
  
    geom_smooth(method = "loess", span = 0.2)+

 
  
  

  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M")) +
  scale_x_continuous(breaks = seq(2, 11 , by=1),labels= c("February","March","April","May","June","July","August","September","October","November")) +
 # scale_x_continuous(labels = scales::dollar_format(scale = 1/1e6,
                                                     #prefix= "",
                                                    # suffix= "February")) +
   
labs(
  title = ("Covid-19 confirmed cases worldwide"),
  subtitle = ("United States has the highest rate of cases"),
  caption = "",
  x = "(Year 2020)",
  y = "Cumulative Cases",
  color = "Country"

) +
  
    
geom_label(aes(label = (totalcases)), 
           hjust = "inward",
           size  = 3,
           color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) 



