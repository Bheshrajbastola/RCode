library(tidyverse)
library(dplyr)
library(GGally)
library("ggplot2")

library(fmsb)

options(scipen = 100000)



unclean_house_2021 = read_csv("houseprice-2021.csv")
colnames(unclean_house_2021) = c("id" , "price", "date", "postCode" , "paon", "saon", "FL", "houseNumber", "type", "streetName",
                                 "locality", "town" , "district", "county", "type1", "type2" );

unclean_house_2020 = read_csv("houseprice-2020.csv")
colnames(unclean_house_2020) = c("id" , "price", "date", "postCode" , "paon", "saon", "FL", "houseNumber", "type", "streetName",
                                 "locality", "town" , "district", "county", "type1", "type2" );

unclean_house_2019 = read_csv("houseprice-2019.csv")
colnames(unclean_house_2019) = c("id" , "price", "date", "postCode" , "paon", "saon", "FL", "houseNumber", "type", "streetName",
                                 "locality", "town" , "district", "county", "type1", "type2" );


unclean_house_prices = unclean_house_2021 %>% 
  add_row(unclean_house_2020) %>% 
  add_row(unclean_house_2019)

write.csv(unclean_house_prices, "uncleanedData/uncleanHousePrice/uncleanHousePrices_2019_2020_2021.csv")







population = read_csv("Population2011_1656567141570.csv", show_col_types = FALSE)

population = population %>% 
  mutate(shortPostCode = substring(Postcode, 1, 4)) %>% 
  group_by(shortPostCode) %>% 
  summarise_at(vars(Population),list(population = sum)) %>%
  mutate(population2019= (1.00561255390388033 * population)) %>%
  mutate(population2020= (1.00561255390388033 * population2019)) %>%
  mutate(population2021= (1.00561255390388033 * population2020)) %>%
  mutate(population2021= (1.00561255390388033 * population2020)) %>%
  mutate(population2022= (1.00561255390388033 * population2021)) %>%
  select(shortPostCode,population2019,population2020,population2021, population2022) %>% 
  na.omit()

colnames(population)
# remove unwanted post code variable
# population = population[,-1]
write.csv(population, "cleanedData/population/cleanPopulation.csv")






#################### towns data set --> cleaning #################### 
towns_unclean = read_csv("uncleanedData/uncleanHousePrice/uncleanHousePrices_2019_2020_2021.csv", show_col_types = FALSE) %>% 
  distinct() %>% 
  na.omit()


cleanPopulation = read_csv("cleanedData/population/cleanPopulation.csv", show_col_types = FALSE)

names(cleanPopulation)
cleanPopulation = cleanPopulation[-1]
towns_unclean = mutate(towns_unclean, shortPostCode = substring(postCode, 1, 4))

towns_clean = towns_unclean %>% 
  filter(county == "LANCASHIRE" | county == "LEICESTERSHIRE") %>% 
  select(postCode, town, district, county, shortPostCode) %>% 
  left_join(cleanPopulation, by = "shortPostCode") %>% 
  na.omit()

colnames(towns_clean)
# saving clean towns data set 
write.csv(towns_clean, "cleanedData/towns/cleanTowns.csv")






unclean_house_prices = read_csv("uncleanedData/uncleanHousePrice/uncleanHousePrices_2019_2020_2021.csv", show_col_types = FALSE)
unclean_house_prices = unclean_house_prices[,-1]

colnames(towns_clean)
towns_clean = read_csv("cleanedData/towns/cleanTowns.csv", show_col_types = FALSE)
towns_clean = towns_clean[,-1]

towns_clean


clean_house_prices = unclean_house_prices %>% 
  filter(county == "LANCASHIRE" | county == "LEICESTERSHIRE") %>% 
  distinct() %>% 
  na.omit() %>% 
  # adding short post and year variables
  mutate(shortPostCode = substring(postCode, 1, 4), year = substring(date, 1, 4)) %>% 
  # selecting only required variables
  select(shortPostCode, price , type, year) %>% 
  na.omit() %>% 
  # joining with clean towns data set
  left_join(towns_clean, by = "shortPostCode") %>% 
  distinct() %>% 
  na.omit() 




# saving clean 2019,2020 and 2021 house prices
write.csv(clean_house_prices, "cleanedData/housePrices/cleanHousePrices2019_2020_2021.csv")


clean_house_prices = read_csv("cleanedData/housePrices/cleanHousePrices2019_2020_2021.csv", show_col_types = FALSE)

housePrice2020_2021 =  clean_house_prices %>% 
  filter(year == 2020 | year == 2021) %>% 
  group_by(town, district, county) %>% 
  summarise(average_price = mean(price)) %>% 
  ungroup(town, district, county) %>% 
  na.omit()




housePrice2020_2021 %>% 
  group_by(district) %>% 
  ggplot(aes(x=district, y=average_price , fill=district)) + 
  scale_y_continuous(limits = c(0,2000000), breaks = seq(0,2000000,200000), label = euro) + 
  geom_boxplot() +coord_flip() + labs(title = "Average house price of 2020 and 2021 by district", x="districts", y="average price") 


housePrice2020_2021 %>% 
  group_by(district) %>% 
  ggplot(aes(x=district)) + geom_bar(aes(fill=average_price, na.rm = TRUE)) + 
  labs(title = "Average house price of 2020 and 2021 by district", x="districts", y="average price")  +
  coord_flip()





clean_house_prices = read_csv("cleanedData/housePrices/cleanHousePrices2019_2020_2021.csv", show_col_types = FALSE)

clean_house_prices =  clean_house_prices %>% 
  group_by(town, district, county, year, price) %>% 
  summarise(average_price = mean(price)) %>% 
  ungroup(town, district, county) %>% 
  na.omit()




clean_house_prices %>% 
  
  group_by(year) %>% 
  summarise(average_price = mean(price)) %>% 
  
  ggplot(aes(x=year, y=average_price)) + geom_line(size=1.5, color="red")  + 
  geom_text(aes(label = average_price), 
            vjust = -0.85) +
  geom_point(size = 2, color = "steelblue")+
  labs(title = "Average house price of 2019, 2020 and 2021 by year", x="year", y="average price")  




# BOXPLOT Average house prices by district (2019-2021)
clean_house_prices %>% 
  group_by(district) %>% 
  ggplot(aes(x = district, y = price, fill=district)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="Average House Price By Year")






































