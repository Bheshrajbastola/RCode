library(tidyverse)
library(dplyr)
library(GGally)
library("ggplot2")

library(fmsb)

options(scipen = 100000)





performance_internet = read_csv("201809_fixed_pc_r03 (1)/201805_fixed_pc_performance_r03.csv", show_col_types = FALSE)


coverage_internet = read_csv("201809_fixed_pc_r03 (1)/201809_fixed_pc_coverage_r01.csv")

colnames(performance_internet)
colnames(coverage_internet)



# saving unclean broadband internet data set
write.csv(performance_internet, "uncleanedData/uncleanBroadband/uncleanBroadband.csv")


# read unclean data set of performance internet
clean_performance_internet = read_csv("uncleanedData/uncleanBroadband/uncleanBroadband.csv", show_col_types = FALSE)



clean_performance_internet = performance_internet %>% 
  mutate(shortPostCode = substring(postcode, 1, 4)) %>% 
  rename(postCode = postcode, avgDownloadSpeed = `Average download speed (Mbit/s)`, maxDownloadSpeed = `Maximum download speed (Mbit/s)`,minDownloadSpeed = `Minimum download speed (Mbit/s)`, avgUploadSpeed = `Average upload speed (Mbit/s)`, maxUploadSpeed = `Maximum upload speed (Mbit/s)`, minUploadSpeed = `Minimum upload speed (Mbit/s)`) %>%
  select(shortPostCode, avgDownloadSpeed, maxDownloadSpeed, minDownloadSpeed, avgUploadSpeed, maxUploadSpeed, minUploadSpeed) %>% 
  na.omit()

# saving the clean broadband internet data set
write.csv(clean_performance_internet, "cleanedData/broadband/cleanbroadbandInternet.csv")  




cleanHousePrices = read_csv("cleanedData/housePrices/cleanHousePrices2019_2020_2021.csv", show_col_types = FALSE) 

cleanHousePrices = cleanHousePrices %>% 
  select(county, town, shortPostCode, district) %>% 
  na.omit()

clean_performance_internet = read_csv("cleanedData/broadband/cleanbroadbandInternet.csv", show_col_types = FALSE)


####### joining Clean House Price and Clean broadband internet data sets

colnames(clean_performance_internet)
clean_internt_housePrice = clean_performance_internet %>% 
  left_join(cleanHousePrices, by="shortPostCode") %>% 
  na.omit()

distinct(select(clean_internt_housePrice, county))




clean_internt_housePrice %>% 
  filter(county == "LANCASHIRE") %>% 
  ggplot(aes(y=town)) + 
  labs(title = "Average and Minimum download internet broadband speed of Lancashire", x = "Intenet Speed (Mbit/s)", y = "Towns") + 
  geom_bar(aes(x=avgDownloadSpeed, fill = "Average"), stat = "Identity") + 
  geom_bar(aes(x=minDownloadSpeed, fill = "Minimum"), stat = "Identity") + 
  guides(fill=guide_legend("Download Speeds"))



clean_internt_housePrice %>% 
  filter(county == "LEICESTERSHIRE") %>% 
  ggplot(aes(y=town)) + 
  labs(title = "Average and Minimum download internet broadband speed of Leicestershire", x = "Intenet Speed (Mbit/s)", y = "Towns") + 
  geom_bar(aes(x=avgDownloadSpeed, fill = "Average"), stat = "Identity") + 
  geom_bar(aes(x=minDownloadSpeed, fill = "Minimum"), stat = "Identity") + 
  guides(fill=guide_legend("Download Speeds"))



clean_internt_housePrice %>% 
  group_by(district) %>% 
  ggplot(aes(x=district, y=avgDownloadSpeed, fill = district)) + 
  labs(title = "Average Download internet speed of Lancashire and Leicestershire", x = "Districts", y="Average Download Speed (Mbits/s)") +
  geom_boxplot() + coord_flip()