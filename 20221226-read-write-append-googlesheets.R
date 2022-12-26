# title: How to Read and Write Google Sheets using R
# author: FelixAnalytix.com


## ----Install and attach R packages---------
if (!require(googlesheets4)) install.packages("googlesheets4")
if (!require(googledrive)) install.packages("googledrive")
if (!require(tidyverse)) install.packages("tidyverse")

library(googlesheets4) # Google Sheets via the Sheets API v4
library(googledrive) # interact with Google Drive
library(tidyverse) # data wrangling and viz


## ----Authentificate to Google Drive--------
googledrive::drive_auth()


## ----List all your Google sheets-----------
gs4_find()


## ----Get a dataset-------------------------
# housing sales in Texas by area and date
df <- ggplot2::txhousing
df


## ----Write Google Sheet--------------------
ss_txhousing <- gs4_create(
  name = "txhousing", 
  sheets = list("Sheet1" = df))

ss_txhousing


## ----Open file in browse-------------------
#gs4_browse(ss_txhousing)


## ----Get sheet metadata--------------------
meta <- gs4_find("txhousing")
meta


## ----Read Google Sheet---------------------
# by URL
url <- paste0("https://docs.google.com/spreadsheets/d/", meta$id)
read_sheet(url)
# by ID
read_sheet(meta$id)
# by name (beware of multiple sheets having the same name => error)
drive_get("txhousing") %>% 
  read_sheet()


## ----Range specification-------------------
gs4_find("txhousing") %>%
  read_sheet(range = "A1:C6")


## ----Create multiple datasets--------------
list_df <- df %>%
  split(f = as.factor(.$city))

df_city_1 <- list_df[[1]]
df_city_2 <- list_df[[2]]
df_city_3 <- list_df[[3]]


## ----Append dataset------------------------
# write data for city 1
ss_cities <- gs4_create(
  name = "cities", 
  sheets = list("Sheet1" = df_city_1))
# append data from city 2
ss_cities %>%
  sheet_append(df_city_2, sheet = "Sheet1")
# read sheet
drive_get("cities") %>% 
  read_sheet() %>%
  count(city)


## ----Add a new worksheet-------------------
gs4_find("cities") %>%
  sheet_add(sheet = "City3", .after = 1) %>%
  sheet_write(data = df_city_3, sheet = "City3")

gs4_find("cities") %>%
  sheet_names()


## ----Rename workseet-----------------------
gs4_find("cities") %>%
  sheet_rename("City3", new_name = "Arlington")

gs4_find("cities") %>%
  sheet_names()


## ----Give access to anyone with URL--------
gs4_find("cities") %>%
  googledrive::drive_share(role = "reader", type = "anyone")


## ----Clean up------------------------------
gs4_find(c("txhousing")) %>%
  googledrive::drive_trash()

gs4_find(c("cities")) %>%
  googledrive::drive_trash()

