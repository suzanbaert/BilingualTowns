library(tidyverse)
library(readxl)
library(sp)
library(BelgiumMaps.StatBel)
library(tmap)

#Data import from http://statbel.fgov.be/nl/statistieken/opendata/datasets/bevolking/big/TF_SOC_POP_STRUCT_2017.jsp
raw_data <- read_excel("TF_SOC_POP_STRUCT_2017_tcm325-283761.xlsx", sheet=1)


#Understanding data structure
glimpse(raw_data)


#Keeping only the variables needed
data <- raw_data %>% 
  select(starts_with("TX_MUNTY"), CD_SEX, TX_NATLTY_NL, TX_CIV_STS_NL, CD_AGE, MS_POPULATION)

colnames(data) <- c("TownNL", "TownFR", "Sex", "Nationality", "MaritalStatus", "Age", "Population")
rm(raw_data)


#Creating a dataframe with total population for each town, and adding a column to see whether they have the same name
popdata <- data %>% 
  group_by(TownNL, TownFR) %>% 
  summarise(population=sum(Population)) %>% 
  arrange(desc(population)) %>%
  mutate(SameName = TownNL==TownFR)


#How many have exactly the same name?
sum(popdata$SameName)
mean(popdata$SameName)

#Which are the ones with different names?
popdata %>% 
  filter(!SameName)


#Where are these towns


  
