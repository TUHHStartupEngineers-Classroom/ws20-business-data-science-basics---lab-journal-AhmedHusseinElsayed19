library(RSQLite)
library(dplyr)
library(httr)
library(glue)
library(jsonlite)
library(keyring)
library(rvest)
library(stringr)
library(purrr)
library(xopen)
library(stringi)
library(tibble)

###########################CHALLENGE###############################
#TASK1

url= "https://api.coinpaprika.com/v1/coins/btc-bitcoin"
resp <- GET(url)
rawToChar(resp$content)

resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

#TASK2
get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    html_text()%>%
    enframe(name = "No.", value = "Bike.Name")
  bike_database_tbl<-bike_url_tbl%>% mutate(price=html_bike_category%>%html_nodes(css =".catalog-category-bikes__price-title")%>% html_text())
}
url= "https://www.rosebikes.de/ebike"
bike_tableout<-get_bike_data(url)
bike_tableout
saveRDS(bike_tableout,"Challenge_TASK2.rds")



