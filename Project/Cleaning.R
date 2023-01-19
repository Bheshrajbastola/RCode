library(tidyverse)
library(dplyr)
library(stringi)
library(scales)
library(ggrepel)


library(GGally)
library("ggplot2")

library(fmsb)
euro <- dollar_format(prefix = "\u20ac", big.mark = ",")


options(scipen = 100000)

HousePrice2019 = read_csv("houseprice-2019.csv", show_col_types = FALSE)
HousePrice2020 = read_csv('houseprice-2020.csv', show_col_types = FALSE)
HousePrice2021 = read_csv('houseprice-2021.csv', show_col_types = FALSE)


colnames(HousePrice2019) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                             "Locality", "Town" , "District", "County", "Type1", "Type2" )

colnames(HousePrice2020) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                             "Locality", "Town" , "District", "County", "Type1", "Type2")

colnames(HousePrice2021) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                             "Locality", "Town" , "District", "County" , "Type1", "Type2")

HousePrices = HousePrice2021 %>%
  add_row(HousePrice2020)%>%
  add_row(HousePrice2019)

write.csv(HousePrices, "UncleanedHousePrices.csv")


# filtering
FilteredHousePrices = filter(HousePrices, County == 'Lancashire' | County == ' Leicestershire')


FilteredHousePrices = FilteredHousePrices %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  select(PostCode,shortPostcode,Year,PAON,Price) %>% 
  na.omit()

write.csv(FilteredHousePrices, "CleanedData/HousePrices.csv")

uncleanedhouseprices = read_csv('UncleanedHousePrices.csv')


housePrice2020_2021 =  HousePrices %>% 
  filter(year == 2020 | year == 2021) %>% 
  group_by(town, district, county) %>% 
  summarise(average_price = mean(price)) %>% 
  ungroup(town, district, county) %>% 
  na.omit()

housePrice2020_2021 %>% 
  group_by(district) %>% 
  ggplot(aes(x=district, y=average_price)) + 
  scale_y_continuous(limits = c(0,2000000), breaks = seq(0,2000000,200000), label = euro) + 
  geom_boxplot() + labs(title = "Average house price of 2020 and 2021 by district", x="districts", y="average price") +
  coord_flip()


housePrice2020_2021 %>% 
  group_by(district) %>% 
  ggplot(aes(x=district)) + geom_bar(aes(fill=average_price, na.rm = TRUE)) + 
  labs(title = "Average house price of 2020 and 2021 by district", x="districts", y="average price")  +
  coord_flip()


Towns = read_csv("CleanedData/Towns.csv")

HousePrices=read_csv("CleanedData/HousePricesclean.csv")

HousePricesclean <- HousePrices %>% 
  left_join(Towns, by ="shortPostcode")

House_town = HousePricesclean %>% 
  filter(County=="Lancashire"|County=="Leicestershire") %>% 
  group_by(Town,District,County) %>% 
  summarise(AveragePrice= mean(Price)) %>% 
  ungroup(Town,District,County) %>%
  na.omit()


# BOXPLOT Average house prices by district (2019-2021)
House_town %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = AveragePrice, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="House Prices by District From 2019-2021")






Population = read_csv("Population2011_1656567141570.csv", show_col_types = FALSE)

FilteredTown = filter(uncleanedhouseprices, County == 'Lancashire' | County == 'Leicestershire')








Population = Population %>%  
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>%
  group_by(shortPostcode) %>%
  summarise_at(vars(Population),list(Population2011 = sum)) %>%
  mutate(Population2012= (1.00695353132322269 * Population2011)) %>%
  mutate(Population2013= (1.00669740535540783 * Population2012)) %>%
  mutate(Population2014= (1.00736463978721671 * Population2013)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2016= (1.00757874492811929 * Population2015)) %>%
  mutate(Population2017= (1.00679374473924223 * Population2016)) %>%
  mutate(Population2018= (1.00605929132212552 * Population2017)) %>%
  mutate(Population2019= (1.00561255390388033 * Population2018)) %>%
  mutate(Population2020= (1.00561255390388033 * Population2019)) %>%
  mutate(Population2021= (1.00561255390388033 * Population2020)) %>%
  select(shortPostcode,Population2019,Population2020,Population2021)


FilteredTown = FilteredTown %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  left_join(Population,by="shortPostcode") %>% 
  select(PostCode, shortPostcode, Year, Town, District, County, Population2019,Population2020,Population2021) %>% 
  group_by(shortPostcode) %>%
  filter(row_number()==1) %>%
  arrange(County) %>% 
  na.omit()

write.csv(FilteredTown, "CleanedData/Towns.csv")



RawBroadband = read_csv("BroadbandSpeeds.csv", show_col_types = FALSE)

FinalBroadbandData = RawBroadband %>%
  mutate(shortPostcode = str_trim(str_sub(postcode_space, -4,-1))) %>% 
  mutate( ID = row_number()) %>% 
  select(ID, `postcode area`, shortPostcode, `Average download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Maximum download speed (Mbit/s)`,
         `Maximum upload speed (Mbit/s)`) %>% 
  na.omit()


write.csv(FinalBroadbandData, "CleanedData/Broadband.csv")


#School Data Set Cleaning



lancashireSchool18 = read_csv('2018-2019_Lancashire.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2018)

lancashireSchool19 = read_csv('2018-2019_Lancashire.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2019)


lancashireSchool21 = read_csv('2021-2022_Lancashire.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2021)

lancashireSchool22 = read_csv('2021-2022_Lancashire.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2022)


leicestershireSchool18 = read_csv('2018-2019_Leicestershire.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2018)

leicestershireSchool19 = read_csv('2018-2019_Leicestershire.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2019)

leicestershireSchool21 = read_csv('2021-2022_Leicestershire.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2021)

leicestershireSchool22 = read_csv('2021-2022_Leicestershire.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2022)



lancashireSchool18 = select(lancashireSchool18, Year, PCODE, SCHNAME, ATT8SCR)
lancashireSchool19 = select(lancashireSchool19, Year, PCODE, SCHNAME, ATT8SCR)
lancashireSchool21 = select(lancashireSchool21, Year, PCODE, SCHNAME, ATT8SCR)
lancashireSchool22 = select(lancashireSchool22, Year, PCODE, SCHNAME, ATT8SCR)

leicestershireSchool18 = select(leicestershireSchool18, Year, PCODE, SCHNAME, ATT8SCR)
leicestershireSchool19 = select(leicestershireSchool19, Year, PCODE, SCHNAME, ATT8SCR)
leicestershireSchool21 = select(leicestershireSchool21, Year, PCODE, SCHNAME, ATT8SCR)
leicestershireSchool22 = select(leicestershireSchool22, Year, PCODE, SCHNAME, ATT8SCR)




lancashireSchoolData = lancashireSchool18 %>% 
  add_row(lancashireSchool19) %>%
  add_row(lancashireSchool21) %>%
  add_row(lancashireSchool22) %>%
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()

colnames(lancashireSchoolData) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")
write.csv(lancashireSchoolData, "CleanedData/lancashireSchoolData.csv") 



leicestershireSchoolData = leicestershireSchool18 %>% 
  add_row(leicestershireSchool19) %>% 
  add_row(leicestershireSchool21) %>% 
  add_row(leicestershireSchool22) %>% 
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()
colnames(leicestershireSchoolData) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")
write.csv(leicestershireSchoolData, "CleanedData/leicestershireSchoolData.csv") 








# CRIME DATA SET CLEANING

#Importing lanchisier data

cd201905g = read_csv('2020-11-lancashire-street.csv', show_col_types = FALSE)
cd201906g = read_csv('2020-12-lancashire-street.csv', show_col_types = FALSE)


#Importing lesisture Side data
#lesisture data

cd201905 = read_csv('2020-11-leicestershire-street.csv', show_col_types = FALSE)
cd201906 = read_csv('2020-12-leicestershire-street.csv', show_col_types = FALSE)
cd201907 = read_csv('2021-01-leicestershire-street.csv', show_col_types = FALSE)
cd201908 = read_csv('2021-02-leicestershire-street.csv', show_col_types = FALSE)
cd201909 = read_csv('2021-03-leicestershire-street.csv', show_col_types = FALSE)
cd201910 = read_csv('2021-04-leicestershire-street.csv', show_col_types = FALSE)
cd201911 = read_csv('2021-05-leicestershire-street.csv', show_col_types = FALSE)
cd202001 = read_csv('./RawData/CrimeData/2020-01-merseyside-street.csv', show_col_types = FALSE)
cd202002 = read_csv('./RawData/CrimeData/2020-02-merseyside-street.csv', show_col_types = FALSE)
cd202003 = read_csv('./RawData/CrimeData/2020-03-merseyside-street.csv', show_col_types = FALSE)
cd202004 = read_csv('./RawData/CrimeData/2020-04-merseyside-street.csv', show_col_types = FALSE)
cd202005 = read_csv('./RawData/CrimeData/2020-05-merseyside-street.csv', show_col_types = FALSE)
cd202006 = read_csv('./RawData/CrimeData/2020-06-merseyside-street.csv', show_col_types = FALSE)
cd202007 = read_csv('./RawData/CrimeData/2020-07-merseyside-street.csv', show_col_types = FALSE)
cd202008 = read_csv('./RawData/CrimeData/2020-08-merseyside-street.csv', show_col_types = FALSE)
cd202009 = read_csv('./RawData/CrimeData/2020-09-merseyside-street.csv', show_col_types = FALSE)
cd202010 = read_csv('./RawData/CrimeData/2020-10-merseyside-street.csv', show_col_types = FALSE)
cd202011 = read_csv('./RawData/CrimeData/2020-11-merseyside-street.csv', show_col_types = FALSE)
cd202012 = read_csv('./RawData/CrimeData/2020-12-merseyside-street.csv', show_col_types = FALSE)
cd202101 = read_csv('./RawData/CrimeData/2021-01-merseyside-street.csv', show_col_types = FALSE)
cd202102 = read_csv('./RawData/CrimeData/2021-02-merseyside-street.csv', show_col_types = FALSE)
cd202103 = read_csv('./RawData/CrimeData/2021-03-merseyside-street.csv', show_col_types = FALSE)
cd202104 = read_csv('./RawData/CrimeData/2021-04-merseyside-street.csv', show_col_types = FALSE)
cd202105 = read_csv('./RawData/CrimeData/2021-05-merseyside-street.csv', show_col_types = FALSE)
cd202106 = read_csv('./RawData/CrimeData/2021-06-merseyside-street.csv', show_col_types = FALSE)
cd202107 = read_csv('./RawData/CrimeData/2021-07-merseyside-street.csv', show_col_types = FALSE)
cd202108 = read_csv('./RawData/CrimeData/2021-08-merseyside-street.csv', show_col_types = FALSE)
cd202109 = read_csv('./RawData/CrimeData/2021-09-merseyside-street.csv', show_col_types = FALSE)
cd202110 = read_csv('./RawData/CrimeData/2021-10-merseyside-street.csv', show_col_types = FALSE)
cd202111 = read_csv('./RawData/CrimeData/2021-11-merseyside-street.csv', show_col_types = FALSE)
cd202112 = read_csv('./RawData/CrimeData/2021-12-merseyside-street.csv', show_col_types = FALSE)

