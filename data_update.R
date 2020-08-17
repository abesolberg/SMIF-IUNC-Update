## Update Fishery & Seabird IUCN Red List Data

## Load Libraries & Functions ----
library(rvest)
library(tidyverse)
library(zoo)
library(RCurl)
library(jsonlite)
library(getPass)

## You will need to get an API token from IUCN to run this script
## You can request the API token here: https://apiv3.iucnredlist.org/api/v3/token

key <- getPass()

# Function to get IUCN Red List Historic Data
getIUCN <- function(x , key) {
  name <- URLencode(x)
  url <- paste0('http://apiv3.iucnredlist.org/api/v3/species/history/name/' , name , 
                '?token=' , key)
  
  X <- getURL(url) ; X <- jsonlite::fromJSON(X)
  results <- X$result ; results$name <- X$name
  return(results)
}

## Get Species Table ----

url <- "https://www.fisheryandseabird.info/documentation/list-determination-taxonomy"

birds <- html_nodes(read_html(url) , "table") %>% 
  .[[2]] %>% 
  html_table(fill = T) %>% 
  mutate(X3 = if_else(X1 == X2, X1 , NA_character_) , 
         family = na.locf(X3)) %>% 
  select(family , scientific_name = X1 , common_name = X2)

birds <- birds %>% filter(family != scientific_name)

## Get Red List Status ----

df <- data.frame()

for (i in 1:nrow(birds)) {
  try(tf <- getIUCN(birds$scientific_name[i] , key = key))
  if (is_list(tf)) tf <- as_data_frame(tf)
    df <- bind_rows(df , tf)
  # Add Sys Sleep to avoid overloading API
  Sys.sleep(1)
  cat(paste(birds$scientific_name[i] , 'Complete\n'))
}

df <- df %>% filter(year >= 2014) 

X <- df %>% arrange(year) %>% 
  distinct() %>% 
  group_by(name , year) %>%
  mutate(row = row_number()) %>%
  pivot_wider(id_cols = c(name , row) , 
              names_from = year , 
              values_from = c(code , category) 
              ) %>% 
  as_tibble() 

X <- X %>% select(name , dplyr::contains('2019') , contains('2018') , contains('2017') , 
             contains('2016') , contains('2015') , contains('2014'))

## Export CSVs ----

write.csv(df , 'IUCN_Update_Long.csv' , row.names = F) 
write.csv(X , 'IUCN_Update_Wide.csv' , row.names = F)

             