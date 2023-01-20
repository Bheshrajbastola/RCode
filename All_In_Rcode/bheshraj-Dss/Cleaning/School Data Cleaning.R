library(tidyverse)
library(dplyr)
library(stringi)
library(scales)


setwd("C:/bheshraj_DS")


School_2016_2017 = fread("Datasets\\2016-2017 England_ks4final.csv",fill=TRUE) %>% 
  mutate(Year = 2016) %>% 
  select( Year,PCODE,SCHNAME, ATT8SCR,) %>%  
  na.omit() %>% 
  distinct()
School_2017_2018 = fread("Datasets\\2017-2018 England_ks4final.csv",fill=TRUE) %>% 
  mutate(Year = 2017) %>% 
  select( Year,PCODE,SCHNAME, ATT8SCR,) %>% 
  na.omit() %>% 
  distinct()
School_2018_2019 = fread("Datasets\\2018-2019 England_ks4final.csv",fill=TRUE) %>% 
  mutate(Year = 2018) %>% 
  select( Year,PCODE,SCHNAME, ATT8SCR,) %>% 
  na.omit() %>% 
  distinct()
School_2021_2022 = fread("Datasets\\2021-2022 England_ks4final.csv",fill=TRUE) %>% 
  mutate(Year = 2021) %>% 
  select( Year,PCODE,SCHNAME, ATT8SCR,) %>% 
  na.omit() %>% 
  distinct()

SchoolData = rbind(School_2016_2017,School_2017_2018,School_2018_2019,School_2021_2022)
pattern = ' .*$'

CleanedSchoolData = SchoolData %>% 
  mutate(ID = row_number()) %>% 
  mutate(shortPostcode=gsub(pattern,"",PCODE)) %>%
  filter (ATT8SCR != "NE" & ATT8SCR != "SUPP") %>% 
  filter(ATT8SCR !=""& shortPostcode!=""& PCODE!="") %>% 
  select( ID,Year,PCODE,shortPostcode,SCHNAME, ATT8SCR,) %>% 
  na.omit() %>% 
  distinct()
View(CleanedSchoolData)

colnames(CleanedSchoolData) = c("ID", "Year", "PostCode", "shortPostcode", "SchoolName", "Attainment8Score")

write.csv(CleanedSchoolData, "Cleaning\\Cleaned Datasets\\Cleaned_School_Data.csv",row.names = FALSE)


Post=read.csv("Cleaning\\Cleaned Datasets\\Combined_House_Pricing_2019-2021.csv") %>% 
  select(PostCode,County) %>% 
  mutate(shortPostcode=gsub(pattern,"",PostCode)) %>% 
  select(County,shortPostcode)

# School data cleaning seperatly for LEICESTERSHIRE and Liverpool

LEICESTERSHIRESchoolData = CleanedSchoolData %>% 
  left_join(Post,by = "shortPostcode") %>% 
  select(Year, PostCode, shortPostcode, SchoolName, Attainment8Score,County) %>% 
  filter(County=="LEICESTERSHIRE") %>% 
  na.omit() %>% 
  distinct() %>% 
  mutate(ID = row_number()) %>% 
  select(ID,Year, PostCode, shortPostcode, SchoolName, Attainment8Score)

write.csv(LEICESTERSHIRESchoolData, "Cleaning/Cleaned datasets/LEICESTERSHIRESchoolData.csv",row.names = FALSE) 

LANCASHIRESchoolData = CleanedSchoolData %>% 
  left_join(Post,by = "shortPostcode") %>% 
  select(Year, PostCode, shortPostcode, SchoolName, Attainment8Score,County) %>% 
  filter(County=="LANCASHIRE") %>% 
  na.omit() %>% 
  distinct() %>% 
  mutate(ID = row_number()) %>% 
  select(ID,Year, PostCode, shortPostcode, SchoolName, Attainment8Score)
View(LANCASHIRESchoolData)

write.csv(LEICESTERSHIRESchoolData, "Cleaning/Cleaned datasets/LANCASHIRESchoolData.csv",row.names = FALSE) 

