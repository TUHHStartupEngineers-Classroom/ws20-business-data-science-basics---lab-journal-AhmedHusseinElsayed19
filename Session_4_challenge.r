#Patents analysis ----
setwd("DS-101/DS_101/01_getting_started/Patent_data_reduced/")
# Importing data: ---- 
library(vroom)
# Tidyverse
library(tidyverse)

# Data Table
library(data.table)

# Counter
library(tictoc)
# 2.0 DATA IMPORT ----

# Patents: ----

col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)



#Assignee_id = id,
# Assignee: ----

col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)


# Patent assignee: ----

col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)


patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_number(),
  sequence = col_number()
)


uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)


# 3.1 Acquisition Data ----

setDT(assignee_tbl)
setDT(patent_tbl)
setDT(patent_assignee_tbl)
setDT(uspc_tbl)

patent_tbl %>% glimpse()
assignee_tbl %>% glimpse()
patent_assignee_tbl %>% glimpse()
uspc_tbl %>% glimpse()


# 4.0 DATA WRANGLING ----

# Target type = 2


# Start the analysis ----
#########################################################################
# Q1.Patent Dominance: What US company / corporation has the most patents? 
# List the 10 US companies with the most assigned/granted patents.
## Output: 
#########################################################################

# 4.1 summarize and count:



setnames(assignee_tbl, "id", "assignee_id")

combined_data <- merge(x = patent_assignee_tbl, y = assignee_tbl, by = "assignee_id")


us_patents <- combined_data %>%
  filter(type == 2)%>%
  filter(!is.na(patent_id) || !is.na(organization)) %>%
  select(-type, -assignee_id)%>% 
  group_by(organization) %>%
  count(patent_id) %>%
  select(-patent_id)%>%
  summarise(total = sum(n))%>%
  arrange(desc(total))   

us_top_10 <- us_patents %>% slice(1:10)


#########################################################################
# Q2. Recent patent acitivity: What US company had the most patents granted in 2019? 
#List the top 10 companies with the most new granted patents for 2019.
#########################################################################


tbl_2 <- patent_tbl %>%   
         separate(col  = date,
         into = c("year", "month", "day"),
          sep  = "-", remove = TRUE) %>%
          mutate(
              month = as.numeric(month)
            )%>%
          filter(month == 01)%>%
          select(-year, -day)

setnames(tbl_2, "id", "patent_id")
combined_data_2 <- merge(x = tbl_2, y = combined_data, by = "patent_id")

us_top10_2014_01 <- combined_data_2%>%
                    filter(type == 2)%>%
                    filter(!is.na(patent_id) || !is.na(organization)) %>%
                    select(organization, patent_id) %>%
                    group_by(organization) %>%
                    count(patent_id) %>%   
                    summarise(total_patents = sum(n))%>%
                    arrange(desc(total_patents)) %>% slice(1:10)  

us_top10_2014_01_new <- combined_data_2%>%
                        filter(type == 2 & num_claims == 1)%>%
                        filter(!is.na(patent_id) || !is.na(organization)) %>%
                        select(organization, patent_id) %>%
                        group_by(organization) %>%
                        count(patent_id) %>%   
                        summarise(total_patents = sum(n))%>%
                        arrange(desc(total_patents)) %>% slice(1:10)
                  
 #########################################################################
# Q. Innovation in Tech: What is the most innovative tech sector? 
# What is the most innovative tech sector? For the top 10 companies (worldwide)
# with the most patents, what are the top 5 USPTO tech main classes?
#########################################################################

combined_data_3 <- merge(x = uspc_tbl, y = combined_data_2, by = "patent_id")



top10_worlwide_patents <- combined_data_3  %>%
                  filter(!is.na(patent_id) || !is.na(organization))%>%
                  group_by(organization) %>%
                  arrange(desc(mainclass_id)) %>% # set mainclass order first, the result will be sorted automatically 
                  count(patent_id) %>%
                  select(-patent_id)%>%
                  summarise(total_patents_wordwide = sum(n))%>%
                  ungroup() %>%
                  arrange(desc(total_patents_wordwide)) %>% slice(1:10)  

top10_worlwid_top5_upts_ <- top10_worlwide %>% slice(1:5)  








