library(tidyverse)
library(readxl)
library(ggplot2)
library(sp)
library(tmap)
library(viridisLite)


#Data import from http://statbel.fgov.be/nl/statistieken/opendata/datasets/bevolking/big/TF_SOC_POP_STRUCT_2017.jsp
raw_data <- read_excel("TF_SOC_POP_STRUCT_2017_tcm325-283761.xlsx", sheet=1)


#Understanding data structure
glimpse(raw_data)


#################
#CLEANING
#################

#Keeping only the variables needed
data <- raw_data %>% 
  select(contains("MUNTY"), TX_RGN_DESCR_NL, CD_SEX, TX_NATLTY_NL, TX_CIV_STS_NL, CD_AGE, MS_POPULATION)

colnames(data) <- c("REFNIS", "TownNL", "TownFR", "Region", "Sex", "Nationality", "MaritalStatus", "Age", "Population")

#Translating Region names to English
data$Region <- data$Region %>% 
  str_replace("Vlaams Gewest", "Flanders") %>% 
  str_replace("Waals Gewest", "Wallonia") %>% 
  str_replace("Brussels Hoofdstedelijk Gewest", "Brussels agglomeration")


#Creating a dataframe with total population for each town, and adding a column to see whether they have the same name
popdata <- data %>% 
  group_by(TownNL, TownFR, Region, REFNIS) %>% 
  summarise(population=sum(Population)) %>% 
  arrange(desc(population)) %>%
  mutate(SameName = TownNL==TownFR) %>% 
  ungroup()


#################
#NOTICED AN ISSUE
#################

#Noticing an issue: townname is sometimes the same but when clarified with their overall district, things got complicated
#Exa,ple: Beveren (Sint-Niklaas) and Beveren (Saint-Nicolas)
popdata%>%
  filter(Region=="Flanders") %>% 
  filter(!SameName) %>% 
  print(n=11)
        

#Go back and create new columns as a copy
popdata <- popdata %>% 
  mutate(cleanTownNL = TownNL) %>% 
  mutate(cleanTownFR = TownFR)

#Searching for the pattern between brackets. In some cases it contains a hypen (e.g Sint-Niklaas)
popdata$cleanTownNL %>% 
  str_view(pattern="\\s\\(.+\\)", match=TRUE)

#Removing the sectors between brackets
popdata$cleanTownNL <- str_replace(popdata$cleanTownNL, pattern="\\s\\(.+\\)", replacement="")
popdata$cleanTownFR <- str_replace(popdata$cleanTownFR, pattern="\\s\\(.+\\)", replacement="")

#Reassessing whether the names are the same, and removing the previous sameName column to avoid confusion
popdata <- popdata %>% 
  mutate(cleanSameName = cleanTownNL==cleanTownFR) %>% 
  select(-SameName) %>% 
  select(cleanTownNL, cleanTownFR, cleanSameName, population, Region, REFNIS)






#################
#DATA EXPLORATION
#################


#How many have exactly the same name?
sum(popdata$cleanSameName)
mean(popdata$cleanSameName)

#by region
popdata %>% 
  group_by(Region) %>% 
  summarise(NTowns=n(), Prop_SameName =round(mean(cleanSameName),2), Prop_DiffName=1-round(mean(cleanSameName),2))

popdata %>% 
  group_by(Region) %>% 
  summarise(sum=sum(cleanSameName), proportion=round(mean(cleanSameName),2))


#Which are the ones with different names?
popdata %>% 
  filter(Region=="Brussels agglomeration") %>% 
  arrange(desc(cleanSameName))

popdata%>%
  filter(Region=="Flanders") %>% 
  filter(!cleanSameName) %>% 
  print(n=nrow(.))

popdata%>%
  filter(Region=="Wallonia") %>% 
  filter(!cleanSameName) %>% 
  print(n=nrow(.))

#It is not just the cities, but also some tiny towns
popdata %>% 
  filter(!cleanSameName) %>% 
  arrange(population)




#Where are these towns? Clearly some small ones but in general they trend much larger than the average town.
require(scales)

ggplot()+
  geom_histogram(data=popdata, aes(x=population), fill="grey", alpha=0.6)+
  geom_histogram(data=subset(popdata, cleanSameName==FALSE), aes(x=population), fill="cadetblue4", alpha=0.8)+
  scale_x_log10()+
  ggtitle("Size of bilingual towns versus all towns")



#Check skew towards cities

#10% largest towns and cities in Belgium
quantile(popdata$population, probs = seq(from = 0, to = 1, by = .1))

popdata %>% 
  filter(population > 50000)%>%
  group_by(cleanSameName)%>% 
  count()
  
popdata %>% 
  filter(population > 50000)%>%
  filter(cleanSameName==TRUE)%>% 
  arrange(desc(population))

popdata %>% 
  filter(Region != "Brussels agglomeration") %>% 
  select(cleanSameName, cleanTownNL, cleanTownFR, population) %>% 
  filter(population > 51348)%>%
  filter(cleanSameName==FALSE)%>% 
  arrange(desc(population)) %>% 
  print(n=nrow(.))




##########
#MAPPING
##########

#Importing SPdataframe for Belgium
data("BE_ADMIN_MUNTY", package="BelgiumMaps.StatBel")
glimpse(BE_ADMIN_MUNTY@data, max.level=2)

#Merging my 2017 data with the SPdataframe
mapdata <- merge(BE_ADMIN_MUNTY, popdata, by.x = "CD_MUNTY_REFNIS", by.y = "REFNIS")
glimpse(mapdata@data, max.level=2)


#trial with only FALSE numbers

popdataFALSEonly <- popdata %>% 
  filter(cleanSameName==FALSE)
  
mapdataFALSEonly <- merge(BE_ADMIN_MUNTY, popdataFALSEonly, by.x = "CD_MUNTY_REFNIS", by.y = "REFNIS")
glimpse(mapdataFALSEonly@data, max.level=2)  



library(RColorBrewer)
library(viridisLite)

palette <- brewer.pal(3, "YlGnBu")
virpalette <- viridis(3)
magpalette <- magma(3)
infpalette <- inferno(3)

#Plot different regions
regionplot<- tm_shape(mapdata) +
  tm_fill(col="Region", palette=rev(virpalette),
          title = "Regions in Belgium")+
  tm_polygons()+
  tm_layout(legend.position = c("left", "bottom"))


#Plot to show those with differnet name by region
nameplot <- tm_shape(mapdataFALSEonly) +
  tm_fill(col="Region", palette=rev(virpalette), id="TownNL", 
          colorNA = "gray90", textNA="Same name", 
          title = "Different regional town names",legend.position = c("left", "bottom" ))+
  tm_polygons()+
  tm_layout(legend.position = c("left", "bottom"))


tmap_arrange(regionplot, nameplot)





map


# library(leaflet)
# leaflet(mapdata) %>%
#   addTiles() %>%
#   addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.85, color = "cleanSameName") %>%
#   addPopups(lng = 4.366354, lat = 50.86619)


#What i would like to change stil
#Draw regions in the back (to see where the language border is)
#Add color for population size
#popup to read the commune name?
  
