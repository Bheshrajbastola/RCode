#school visualization

library(tidyverse)
library(dplyr)
library(GGally)
library("ggplot2")

library(fmsb)

options(scipen = 100000)

schoolData = read_csv('School.csv', show_col_types = FALSE)

leicestershireSchool = read_csv("leicestershireSchoolData.csv")
lancashireSchool = read_csv("lancashireSchoolData.csv")



schoolData %>% 
  group_by(Year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment), 
            vjust = -0.85) +
  scale_x_continuous(breaks = 2016:2019) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")


# Boxplot of year 2016-2019 where Attainment8Score is greater than 30
schoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot(color="purple") +
  coord_flip() +
  labs(title="2016-2019 Attainment8Score of Schools")
schoolData


# Boxplot of year 2016-2019 where Attainment8Score is greater than 30 (Leicester shire SCHOOL ONLY)
leicestershireSchool %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot(fill="yellow") +
  coord_flip() +
  labs(title="2019 Average Attainment8Score of Leicestershire Schools")

# Box plot of year 2016-2019 where Attainment8Score is greater than 30 (Lanca shire SCHOOL ONLY)
lancashireSchool %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot(fill="blue") +
  coord_flip() +
  labs(title="2016-2019 Average Attainment8Score of Lancashire Schools")




