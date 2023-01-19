library(tidyverse)
library(dplyr)
library(GGally)
library("ggplot2")

library(fmsb)

options(scipen = 100000)


Towns = read_csv("Cleaned Data/Towns.csv")
Crimes = read_csv("Cleaned Data/cleanCrimes.csv") 
Crimes = Crimes %>% left_join(Towns, by = "shortPostcode") %>% 
  filter(CrimeType=="Drugs") %>% 
  group_by(Year,CrimeType,County) %>%
  summarise(sum(n),sum(Population2020)) %>%
  mutate(Rate = (sum(n) / sum(Population2020))*10000) %>% 
  arrange(Rate) %>% ungroup(Year,CrimeType,County) %>%
  mutate(row=row_number()) colors <- c(“Lancashire” = "red", "Leicestershire" = "blue") 
  ggplot(data=Crimes, aes(x=Year, y=Rate)) + geom_line(data=filter(Crimes,County=="LEICESTERSHIRE"),aes(color="Leicestershire"))+ 
  geom_line(data=filter(Crimes,County==“LANCASHIRE”),aes(color=“Lancashire”))+ 
  labs(x="Year",y="Rate",title="Drug Offense Rates (per 10,000 people)",color="County") 


library(tidyverse) 
Towns = read_csv("Cleaned Data/Towns.csv") 
Crimes = read_csv("Cleaned Data/cleanCrimes.csv") 
Crimes = Crimes %>% left_join(Towns, by = "shortPostcode") %>% 
  filter(Year=="2020",CrimeType=="Drugs") %>%
  mutate(Rate = (n /Population2020)*10000) Crimes ggplot(data=Crimes, aes(x=Rate, y=District)) + 
  geom_boxplot(outlier.colour="red")+ scale_x_continuous(limits=c(0,20), breaks=seq(0,20,5)) + 
  labs(x="Rate",y="District",title="Drug Offenses 2020")