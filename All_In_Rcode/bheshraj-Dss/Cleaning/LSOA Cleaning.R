library(data.table)
library(tidyverse)
library(dplyr)


## Cleaning here

getwd()
setwd("C:/bheshraj_DS")

Cleaned_HP = read.csv("Cleaning/Cleaned Datasets/Cleaned_House_Pricing_2019-2021.csv" )
LSOA = read.csv("Datasets/Postcode to LSOA.csv")
pattern = ' .*$'
LSOA_Cleaned = LSOA %>%
  select(lsoa21cd,pcds) %>% 
  mutate(SHORTPOSTCODE=gsub(pattern,"",pcds)) %>% 
  right_join(Cleaned_HP,by="SHORTPOSTCODE")  %>% 
  group_by(lsoa21cd) %>% 
  select(lsoa21cd,SHORTPOSTCODE,TOWN,DISTRICT,COUNTY) %>% 
  filter(SHORTPOSTCODE!="" & lsoa21cd!="" & TOWN!="" & DISTRICT!="" & COUNTY!="") %>% 
  distinct()
colnames(LSOA_Cleaned)[1] <- "LSOA code"

write.csv(LSOA_Cleaned,"Cleaning/Cleaned Datasets/Cleaneds_LSOA.csv",row.names = FALSE,col.names = FALSE)

