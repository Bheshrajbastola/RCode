library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)

setwd("C:/bheshraj_DS")
# Importing CSV files
Crime_2019_11_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2019-11\\2019-11-lancashire-street.csv")
Crime_2019_12_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2019-12\\2019-12-lancashire-street.csv")
Crime_2020_01_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-01\\2020-01-lancashire-street.csv")
Crime_2020_02_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-02\\2020-02-lancashire-street.csv")
Crime_2020_03_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-03\\2020-03-lancashire-street.csv")
Crime_2020_04_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-04\\2020-04-lancashire-street.csv")
Crime_2020_05_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-05\\2020-05-lancashire-street.csv")
Crime_2020_06_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-06\\2020-06-lancashire-street.csv")
Crime_2020_07_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-07\\2020-07-lancashire-street.csv")
Crime_2020_08_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-08\\2020-08-lancashire-street.csv")
Crime_2020_09_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-09\\2020-09-lancashire-street.csv")
Crime_2020_10_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-10\\2020-10-lancashire-street.csv")
Crime_2020_11_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-11\\2020-11-lancashire-street.csv")
Crime_2020_12_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-12\\2020-12-lancashire-street.csv")
Crime_2021_01_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-01\\2021-01-lancashire-street.csv")
Crime_2021_02_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-02\\2021-02-lancashire-street.csv")
Crime_2021_03_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-03\\2021-03-lancashire-street.csv")
Crime_2021_04_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-04\\2021-04-lancashire-street.csv")
Crime_2021_05_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-05\\2021-05-lancashire-street.csv")
Crime_2021_06_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-06\\2021-06-lancashire-street.csv")
Crime_2021_07_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-07\\2021-07-lancashire-street.csv")
Crime_2021_08_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-08\\2021-08-lancashire-street.csv")
Crime_2021_09_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-09\\2021-09-lancashire-street.csv")
Crime_2021_10_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-10\\2021-10-lancashire-street.csv")
Crime_2021_11_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-11\\2021-11-lancashire-street.csv")
Crime_2021_12_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-12\\2021-12-lancashire-street.csv")
Crime_2022_01_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-01\\2022-01-lancashire-street.csv")
Crime_2022_02_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-02\\2022-02-lancashire-street.csv")
Crime_2022_03_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-03\\2022-03-lancashire-street.csv")
Crime_2022_04_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-04\\2022-04-lancashire-street.csv")
Crime_2022_05_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-05\\2022-05-lancashire-street.csv")
Crime_2022_06_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-06\\2022-06-lancashire-street.csv")
Crime_2022_07_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-07\\2022-07-lancashire-street.csv")
Crime_2022_08_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-08\\2022-08-lancashire-street.csv")
Crime_2022_09_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-09\\2022-09-lancashire-street.csv")
Crime_2022_10_lancashire_ = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-10\\2022-10-lancashire-street.csv")

Crime_2019_11_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2019-11\\2019-11-leicestershire-street.csv")
Crime_2019_12_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2019-12\\2019-12-leicestershire-street.csv")
Crime_2020_01_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-01\\2020-01-leicestershire-street.csv")
Crime_2020_02_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-02\\2020-02-leicestershire-street.csv")
Crime_2020_03_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-03\\2020-03-leicestershire-street.csv")
Crime_2020_04_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-04\\2020-04-leicestershire-street.csv")
Crime_2020_05_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-05\\2020-05-leicestershire-street.csv")
Crime_2020_06_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-06\\2020-06-leicestershire-street.csv")
Crime_2020_07_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-07\\2020-07-leicestershire-street.csv")
Crime_2020_08_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-08\\2020-08-leicestershire-street.csv")
Crime_2020_09_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-09\\2020-09-leicestershire-street.csv")
Crime_2020_10_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-10\\2020-10-leicestershire-street.csv")
Crime_2020_11_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-11\\2020-11-leicestershire-street.csv")
Crime_2020_12_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2020-12\\2020-12-leicestershire-street.csv")
Crime_2021_01_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-01\\2021-01-leicestershire-street.csv")
Crime_2021_02_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-02\\2021-02-leicestershire-street.csv")
Crime_2021_03_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-03\\2021-03-leicestershire-street.csv")
Crime_2021_04_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-04\\2021-04-leicestershire-street.csv")
Crime_2021_05_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-05\\2021-05-leicestershire-street.csv")
Crime_2021_06_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-06\\2021-06-leicestershire-street.csv")
Crime_2021_07_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-07\\2021-07-leicestershire-street.csv")
Crime_2021_08_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-08\\2021-08-leicestershire-street.csv")
Crime_2021_09_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-09\\2021-09-leicestershire-street.csv")
Crime_2021_10_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-10\\2021-10-leicestershire-street.csv")
Crime_2021_11_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-11\\2021-11-leicestershire-street.csv")
Crime_2021_12_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2021-12\\2021-12-leicestershire-street.csv")
Crime_2022_01_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-01\\2022-01-leicestershire-street.csv")
Crime_2022_02_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-02\\2022-02-leicestershire-street.csv")
Crime_2022_03_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-03\\2022-03-leicestershire-street.csv")
Crime_2022_04_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-04\\2022-04-leicestershire-street.csv")
Crime_2022_05_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-05\\2022-05-leicestershire-street.csv")
Crime_2022_06_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-06\\2022-06-leicestershire-street.csv")
Crime_2022_07_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-07\\2022-07-leicestershire-street.csv")
Crime_2022_08_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-08\\2022-08-leicestershire-street.csv")
Crime_2022_09_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-09\\2022-09-leicestershire-street.csv")
Crime_2022_10_leicestershire = fread("Datasets\\Crime Data Nov 2019 to Oct 2022\\2022-10\\2022-10-leicestershire-street.csv")

Crime_combined = rbind(Crime_2019_11_lancashire_, Crime_2019_12_lancashire_,Crime_2020_01_lancashire_,Crime_2020_02_lancashire_,
                       Crime_2020_03_lancashire_,Crime_2020_04_lancashire_,Crime_2020_05_lancashire_,Crime_2020_06_lancashire_,
                       Crime_2020_07_lancashire_,Crime_2020_08_lancashire_,Crime_2020_09_lancashire_,Crime_2020_10_lancashire_,
                       Crime_2020_11_lancashire_,Crime_2020_12_lancashire_, Crime_2021_01_lancashire_, Crime_2021_02_lancashire_, 
                       Crime_2021_03_lancashire_, Crime_2021_04_lancashire_,Crime_2021_05_lancashire_, Crime_2021_06_lancashire_,
                       Crime_2021_07_lancashire_, Crime_2021_08_lancashire_, Crime_2021_09_lancashire_,
                       Crime_2021_10_lancashire_,Crime_2021_11_lancashire_,Crime_2021_12_lancashire_, Crime_2022_01_lancashire_,
                       Crime_2022_02_lancashire_,Crime_2022_03_lancashire_,Crime_2022_04_lancashire_,Crime_2022_05_lancashire_,
                       Crime_2022_06_lancashire_,Crime_2022_07_lancashire_,Crime_2022_08_lancashire_,Crime_2022_09_lancashire_,
                       Crime_2022_10_lancashire_,Crime_2019_11_leicestershire,Crime_2019_12_leicestershire,Crime_2020_01_leicestershire,
                      Crime_2020_02_leicestershire,Crime_2020_03_leicestershire,Crime_2020_04_leicestershire,Crime_2020_05_leicestershire,
                      Crime_2020_06_leicestershire,Crime_2020_07_leicestershire,Crime_2020_08_leicestershire,Crime_2020_09_leicestershire,
                      Crime_2020_10_leicestershire,Crime_2020_11_leicestershire,Crime_2020_12_leicestershire,Crime_2021_01_leicestershire,
                      Crime_2021_02_leicestershire,Crime_2021_03_leicestershire,Crime_2021_04_leicestershire,Crime_2021_05_leicestershire,
                      Crime_2021_06_leicestershire,Crime_2021_07_leicestershire,Crime_2021_08_leicestershire,Crime_2021_09_leicestershire,
                      Crime_2021_10_leicestershire,Crime_2021_11_leicestershire,Crime_2021_12_leicestershire,Crime_2022_01_leicestershire,
                      Crime_2022_02_leicestershire,Crime_2022_03_leicestershire,Crime_2022_04_leicestershire,Crime_2022_05_leicestershire,
                      Crime_2022_06_leicestershire,Crime_2022_07_leicestershire,Crime_2022_08_leicestershire,Crime_2022_09_leicestershire,
                      Crime_2022_10_leicestershire)

Crime_combined<- Crime_combined %>% 
  as_tibble()

write.csv(Crime_combined,"Cleaning/cleaned datasets/Combined_Crime_Data.csv",row.names = FALSE)


#cleaning crime data
crimedata = read_csv('Cleaning/cleaned datasets/Combined_Crime_Data.csv') %>% 
  select(Month, `LSOA code`, `Crime type`)

colnames(crimedata) = c("Year", "LSOA.code", "CrimeType")

pattern = ' .*$'
LsoaToPostcode = read_csv('Cleaning/cleaned datasets/Cleaned_LSOA.csv')
colnames(LsoaToPostcode)=c("LSOA.code","shortPostcode","Town","District","County")
view(crimedata)
View(LsoaToPostcode)



crimedataCleaned = crimedata %>%
  left_join(LsoaToPostcode,by="LSOA.code") %>% 
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  group_by(shortPostcode,CrimeType,Year)%>% 
  select(shortPostcode, Year, CrimeType, ) %>% 
  na.omit() %>% 
  tally() %>% 
  as_tibble()


crimedataCleaned = cbind(ID = 1:nrow(crimedataCleaned), crimedataCleaned)  
colnames(crimedataCleaned)= c("ID","shortPostcode","CrimeType","Year" , "CrimeCount")
View(crimedataCleaned)

write.csv(crimedataCleaned, "Cleaning/cleaned datasets/Cleaned_Crime_Data.csv",row.names = FALSE)

