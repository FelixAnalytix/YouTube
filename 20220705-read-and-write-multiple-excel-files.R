# title: How to read (and write) multiple Excel files using R
# author: FelixAnalytix.com


# INSTALL AND LOAD R PACKAGES ---------------------------------------------

if (!require(writexl)) install.packages("writexl")
if (!require(readxl)) install.packages("readxl")
if (!require(tidyverse)) install.packages("tidyverse")

library(writexl)
library(readxl)
library(tidyverse)


# WRITE AND READ A SINGLE EXCEL FILE --------------------------------------

# housing sales in Texas by area and date
ggplot2::txhousing

# Create folder where Excel files will be saved
dir.create("txhousing")

# write a data frame as an XLSX Excel file
writexl::write_xlsx(txhousing, path = "txhousing/txhousing.xlsx")

# read an XLSX Excel file
df <- readxl::read_excel(path = "txhousing/txhousing.xlsx")

# Remove Excel file
unlink("txhousing/txhousing.xlsx")


# WRITE MULTIPLE EXCEL FILES ----------------------------------------------

# Function which writes Excel files
write_excel_file <- function(city_name) {
  txhousing %>%
    filter(city == city_name) %>% # filter by city
    writexl::write_xlsx(path = paste0("txhousing/txhousing_", city_name, ".xlsx"))
}

# Test on 1st city
write_excel_file(city_name = txhousing$city[1])

# list of unique city names
list_city_names <- unique(txhousing$city)

# Loop to create multiple Excel files
purrr::map(.x = list_city_names, write_excel_file)


# READ AND JOIN MULTIPLE EXCEL FILES --------------------------------------

# get list files
list_excel_files <- list.files(path = "txhousing", full.names = TRUE)

# Read Excel file and add `file_path` variable
read_excel_file <- function(file) {
  if(is.na(file)) stop("No file path") # test if path exists
  
  df <- readxl::read_excel(file)
  # add data cleaning/validation here
  df
}

# Test on 1st Excel file
read_excel_file(file = list_excel_files[1])

# Loop to read all Excel files data
df_list <- purrr::map(.x = list_excel_files, .f = read_excel_file)

# get 1st data frame
df_1 <- df_list[[1]]

# Loop to read and bind/merge all data frames by row
df_merged <- purrr::map_dfr(.x = list_excel_files, read_excel_file)

# Remove folder and all Excel files
unlink("txhousing", recursive = TRUE)
