library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)

setwd(":C/bheshraj_DS")

euro <- dollar_format(prefix = "\u20ac", big.mark = ",")

#-------------------------graph of broadband speed---------------------
Towns = read_csv("Cleaning/Cleaned datasets/Cleaned_Town_population.csv")%>% 
  select(shortPostcode, Town, District, County)
BroadbandSpeedsclean=read_csv("Cleaning/Cleaned datasets/Cleaned_Broadband_Speed.csv")
view(BroadbandSpeedsclean)
view(Towns)

broadband=Towns %>% 
  left_join(BroadbandSpeedsclean,by="shortPostcode")
View(broadband)

ggplot(broadband,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title=" LANCASHIRE Broadband Speeds")+
  geom_bar(data=filter(broadband,County=="LANCASHIRE"),aes(x=Avgdownload,fill="Average"),stat="Identity")+
  geom_bar(data=filter(broadband,County=="LANCASHIRE"),aes(x=Mindownload,fill="Minimum"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))

#---------------LEICESTERSHIRE Broadband Speeds---------------
ggplot(broadband,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title="LEICESTERSHIRE Broadband Speeds")+
  geom_bar(data=filter(broadband,County=="LEICESTERSHIRE"),aes(x=Avgdownload,fill="Average"),stat="Identity")+
  geom_bar(data=filter(broadband,County=="LEICESTERSHIRE"),aes(x=Mindownload,fill="Minimum"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))

#---------------Average download speed---------------
broadband %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = `Avgdownload`, fill=District)) +
  scale_y_continuous(breaks = seq(0,200,10)) +
  geom_boxplot() +
  labs(title = "Average download speed (Mbit/s) by district", x = "District",
       y = "Average Download Speed (Mbit/s)")+
  coord_flip()

#--------------broadband end-----------------------------


#---------------graph of Average house price ------------------
Towns = read_csv("Cleaning/Cleaned datasets/Cleaned_Town_population.csv") %>% 
  select(-Year) 
View(Towns)
HousePrices=read_csv("Cleaning/Cleaned datasets/Cleaned_House_Pricing_2019-2021.csv", show_col_types = FALSE)

HousePricesclean <- HousePrices %>% 
  select(-PostCode) %>% 
  left_join(Towns, by ="shortPostcode")

House_town = HousePricesclean %>% 
  filter(County=="LEICESTERSHIRE"|County=="LANCASHIRE") %>% 
  group_by(Town,District,County) %>% 
  summarise(AveragePrice= mean(Price)) %>% 
  ungroup(Town,District,County) %>%
  na.omit()
view(HousePricesclean)

# BOXPLOT Average house prices by district (2019-2021)
House_town %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = AveragePrice, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2021 house prices by district")

# BARGRAPH houseprices by district (2021)
HousePricesclean %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = District, y = AveragePrice)) +
  geom_bar(position = "stack",stat = "identity", fill = "cornflowerblue") +
  scale_y_continuous(limits=c(0,5000000),breaks = seq(0, 5000000, 30000), 
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.25) +
  labs(title = "2021 Average house prices by district") +
  coord_flip()

#LINEGRAPH Average house prices by year (2019-2021)
HousePricesclean %>% 
  group_by(Year) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = Year, y = AveragePrice)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.85) +
  scale_y_continuous(breaks = seq(0, 300000, 5000), 
                     label = euro) +
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "2019-2021 Average house prices by year")

# BOXPLOT Average house prices by district (2019-2021)
HousePricesclean %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2021 house prices by district")


# BOXPLOT Average house prices by district (2021)
HousePricesclean %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2021 house prices by district")

#-------------end------------------------------------------

#-----------crime graph-------------------

Town  = read_csv("cleaned dataset/Towns.csv")
crime_Data = read_csv("cleaned dataset/Crime.csv")

View(Town)
crimeData = crime_Data %>% 
  select(-Year) %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit()
View(crimeData)

# Boxplot for 2019-2021 Drugs count by District
crimeData %>% 
  filter(CrimeType == "Drugs") %>% 
  ggplot(aes(x=District, y=CrimeCount, fill=CrimeType)) + 
  geom_boxplot() +
  labs(title=" 2019-2021 Drugs count by District")+
  coord_flip()


# Piechart for 2021 Robbery by District
RobberyData <- crimeData %>% 
  filter(CrimeType=="Robbery", Year == 2021) %>%
  group_by(Town) %>%
  mutate(sumCount = sum(CrimeCount)) %>% 
  ungroup() %>%
  mutate(perc =sumCount / sum(CrimeCount)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(Town, sumCount, perc, labels) %>% 
  select(Town, sumCount, perc, labels)

RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill = Town)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  theme_void()+
  labs(title="2021 Robbery by District")

schoolData = read_csv('cleaned dataset/Cleaned_School_Data.csv', show_col_types = FALSE)

LANCASHIRESchool = read_csv("cleaned dataset/LANCASHIRESchoolData.csv")
LEICESTERSHIRESchoolData = read_csv('cleaned dataset/LEICESTERSHIRESchoolData.csv')
view(schoolData)

# Linegraph Average Attainment8Score by year
schoolData %>% 
  group_by(Year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment), 
            vjust = -0.85) +
  scale_x_continuous(breaks = 2017:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")


# Boxplot of year 2017-2021 where Attainment8Score is greater than 30
schoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2017-2021 Attainment8Score of Schools")


# Boxplot of year 2017-2021 where Attainment8Score is greater than 30 (LANCASHIRE SCHOOL ONLY)
LANCASHIRESchool %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot(fill="purple") +
  coord_flip() +
  labs(title="2017-2021 Average Attainment8Score of LANCASHIRE Schools")



# Boxplot of year 2017-2021 where Attainment8Score is greater than 30 (LEICESTERSHIRE SCHOOL ONLY)
LEICESTERSHIRESchoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot(fill="green") +
  coord_flip() +
  labs(title="2017-2021 Average Attainment8Score of LEICESTERSHIRE Schools")


#liner regression 
Towns = read_csv("Cleaning/Cleaned datasets/Cleaned_Town_population.csv")%>% 
  select(shortPostcode, Town, District, County)
prices = read_csv("Cleaning/cleaned datasets/Cleaned_House_Pricing_2019-2021.csv")

speeds = read_csv("Cleaning/Cleaned datasets/Cleaned_Broadband_Speed.csv") %>% 
  na.omit()  

crime=read_csv("Cleaning/cleaned datasets/Cleaned_Crime_Data.csv")

schools=read_csv("Cleaning\\Cleaned Datasets\\Cleaned_School_Data.csv") %>% 
  na.omit()
#------------------------------House prices vs Download Speed----------------------------------------

options(scipen=999)

HousePrices = prices %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  summarise(AverageDownload=mean(Avgdownload))

lm_res = HousePrices %>% left_join(BroardbandSpeeds,by="Town")
model = lm(data= lm_res, Price~AverageDownload)
summary(model)

color= c("LEICESTERSHIRE" = "red", "LANCASHIRE" = "blue")

ggplot(lm_res,aes(x=AverageDownload,y=Price)) +
  geom_point(data = filter(lm_res,County.x=="LANCASHIRE"),aes(color="LANCASHIRE"))+
  geom_point(data = filter(lm_res,County.x=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="lightgreen")+
  labs(x="Download Speed (Mbit/s)",y="Price",title="House Prices vs Download Speed",color="County")

#-----------------------------------------------------------------------------------------
#----------------------------------House price and drug offence--------------------------------------------------

HousePrices = prices %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res1 = HousePrices %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model1 = lm(data= lm_res1, Price~CrimeCount)
summary(model1)

color= c("LEICESTERSHIRE" = "yellow", "LANCASHIRE" = "Green")

ggplot(lm_res1,aes(x=CrimeCount,y=Price)) +
  geom_point(data = filter(lm_res1,County.x=="LANCASHIRE"),aes(color="LANCASHIRE"))+
  geom_point(data = filter(lm_res1,County.x=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="peachpuff4")+
  labs(x="count",y="Price",title="House Prices vs Drug",color="County")

#----------------------------Drug and  school -----------------------------------
school_lm= schools%>%
  left_join(Towns,by=c("shortPostcode"="shortPostcode") )%>%  
  group_by(Town,County) %>%
  summarise(score=mean(Attainment8Score)) 
Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res2 = school_lm %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model2 = lm(data= lm_res2, CrimeCount~score)
summary(model2)

colors1= c("LEICESTERSHIRE" = "yellow", "LANCASHIRE" = "Green")

ggplot(lm_res2,aes(x=CrimeCount,y=score)) +
  geom_point(data = filter(lm_res2,County.x=="LEICESTERSHIRE"),aes(color="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res2,County.x=="LANCASHIRE"), aes(color="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="peachpuff4")+
  labs(x="count",y="score",title="Attainment Score vs Drug Offence Rate",color="County")

#---------------------------------Average download and drug-------------------
BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  summarise(AverageDownload=mean(Avgdownload))

Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res3 = BroardbandSpeeds %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model3 = lm(data= lm_res3, AverageDownload~CrimeCount)
summary(model3)

colors1= c("LEICESTERSHIRE" = "yellow", "LANCASHIRE" = "Green")

ggplot(lm_res3,aes(x=CrimeCount,y=AverageDownload)) +
  geom_point(data = filter(lm_res3,County.x=="LANCASHIRE"),aes(color="LANCASHIRE"))+
  geom_point(data = filter(lm_res3,County.x=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="peachpuff4")+
  labs(x="count",y="Average Download",title="Average Download vs Drug",color="County")

#-----------------------average dwnload and school-------------------------

BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  summarise(AverageDownload=mean(Avgdownload))
school_lm= schools %>%
  left_join(Towns,by=c("shortPostcode"="shortPostcode") )%>%  
  group_by(Town,County) %>%
  summarise(score=mean(Attainment8Score))

HousePrices = prices %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

lm_res4 = BroardbandSpeeds %>% left_join(school_lm,by="County") %>% 
  na.omit()
model4 = lm(data= lm_res4, AverageDownload~score)
summary(model4)

colors1= c("LEICESTERSHIRE" = "yellow", "LANCASHIRE" = "Green")
ggplot(lm_res4,aes(x=score,y=AverageDownload)) +
  geom_point(data = filter(lm_res4,County=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="peachpuff4")+
  geom_point(data = filter(lm_res4,County=="LANCASHIRE"),aes(color="LANCASHIRE"))+
  labs(x="Score",y="Average Download",title="Average Download vs School",color="County")

#--------------------House Prices and School----------------------------------------
school_lm= schools %>%
  left_join(Towns,by=c("shortPostcode"="shortPostcode") )%>%  
  group_by(Town,County) %>%
  summarise(score=mean(Attainment8Score)) 
HousePrices = prices %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

lm_res4 = HousePrices %>% left_join(school_lm,by="County") %>% 
  na.omit()
model4 = lm(data= lm_res4, Price~score)
summary(model4)
colors1= c("LEICESTERSHIRE" = "yellow", "LANCASHIRE" = "Green")
ggplot(lm_res4,aes(x=score,y=Price)) +
  geom_point(data = filter(lm_res4,County=="LANCASHIRE"),aes(color="Greater Mancehster"))+
  geom_point(data = filter(lm_res4,County=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="gold")+
  labs(x="Score",y="Average House Price",title="Average House Price vs Attainment at School",color="County")
colors1= c("LEICESTERSHIRE" = "yellow", "LANCASHIRE" = "Green")

ggplot(lm_res4,aes(x=score,y=Price)) +
  geom_point(data = filter(lm_res4,County=="LANCASHIRE"),aes(color="Greater Mancehster"))+
  geom_point(data = filter(lm_res4,County=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="gold")+
  labs(x="Score",y="Average House Price",title="Average House Price vs Attainment at School",color="County")
