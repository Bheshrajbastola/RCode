library(tidyverse)
library(dplyr)
library(stringi)
library(scales)

setwd("C:/bheshraj_DS")

# BROADBAND DATA CLEANING
Broadband = read_csv("Datasets/Broadband Speed 2018.csv", show_col_types = FALSE)


pattern = ' .*$'
BroadbandData = Broadband %>%
  mutate(shortPostcode=gsub(pattern,"",postcode_space)) %>% 
  mutate(ID = row_number()) %>% 
  select(`ID`, `postcode area`, shortPostcode, `Average download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Minimum download speed (Mbit/s)`,
         `Minimum upload speed (Mbit/s)`) %>% 
  na.omit()
colnames(BroadbandData) = c( "ID","postcode area", "shortPostcode", "Avgdownload",
                             "Average upload speed (Mbit/s)", "Mindownload",
                             "Minimum upload speed (Mbit/s)")
write.csv(BroadbandData, "Cleaning/cleaned datasets/Cleaned_Broadband_Speed.csv",row.names = FALSE)
