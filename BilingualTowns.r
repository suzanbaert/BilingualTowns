library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(sp)
library(tmap)
library(viridisLite)
library(readr)


#Data import from http://statbel.fgov.be/nl/statistieken/opendata/datasets/bevolking/big/TF_SOC_POP_STRUCT_2017.jsp
#raw_data <- read_excel("TF_SOC_POP_STRUCT_2017_tcm325-283761.xlsx", sheet=1)

#CSV test did not work
raw_data <- read_csv("2017-12 TF_SOC_POP_STRUCT_2017_tcm325-283761.csv")
Encoding (raw_data$TX_MUNTY_DESCR_NL) <- "latinl"
Encoding (raw_data$TX_MUNTY_DESCR_FR) <- "latinl"

#check for strange characters
filter(raw_data, grepl("bach", TX_MUNTY_DESCR_NL))
filter(raw_data, grepl("Ambl", TX_MUNTY_DESCR_FR))



# library(XLConnect)
# path <- "https://github.com/suzanbaert/BlogFiles/blob/master/content/data/2017-12%20TF_SOC_POP_STRUCT_2017_tcm325-283761.csv?raw=true"
# 
# # Load workbook
# wb <- loadWorkbook(path)
# raw_data <- readWorksheet(wb, sheet = 1)
# 
# read.csv()


#Some unwanted side effects by changing excel to csv las minute


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


data %>%
  filter(TownNL=="Kapelle-op-den-Bos")%>%
  filter(Sex=="F")%>%
  filter(MaritalStatus=="Ongehuwd")%>%
  filter(Age==34)

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
        

#Searching for the pattern between brackets. In some cases it contains a hypen (e.g Sint-Niklaas)
popdata$TownNL %>% 
  str_view(pattern="\\s\\(.+\\)", match=TRUE)

#Removing the sectors between brackets
popdata$TownNL <- str_replace(popdata$TownNL, pattern="\\s\\(.+\\)", replacement="")
popdata$TownFR <- str_replace(popdata$TownFR, pattern="\\s\\(.+\\)", replacement="")

#Reassessing whether the names are the same, and removing the previous sameName column to avoid confusion
popdata <- popdata %>% 
  mutate(DiffName = TownNL != TownFR) %>%
  select(TownNL, TownFR, DiffName, population, Region, REFNIS)






#################
#DATA EXPLORATION
#################


#How many have exactly the same name?
sum(popdata$DiffName)
mean(popdata$DiffName)

#by region
popdata %>% 
  group_by(Region) %>% 
  summarise(NTowns=n(), N_SameName=n()-sum(DiffName), N_DiffName=sum(DiffName), 
           Prop_SameName =1-round(mean(DiffName),2), Prop_DiffName=round(mean(DiffName),2))


### REASON 1
#First obvious reason is being a commune of the Brussels agglomeration, an official bilingual region

popdata %>% 
  filter(Region=="Brussels agglomeration") %>% 
  summarise(NTowns=n(), N_SameName=n()-sum(DiffName), N_DiffName=sum(DiffName), 
            Prop_SameName =1-round(mean(DiffName),2), Prop_DiffName=round(mean(DiffName),2))


popdata %>% 
  filter(Region=="Brussels agglomeration") %>% 
  group_by(DiffName) %>%
  arrange(desc(DiffName), desc(population))

  
#Adding a column to note down the reason for different names
reason_BXL <- popdata %>% 
  filter(Region=="Brussels agglomeration") %>% 
  filter(DiffName) %>%
  mutate(Reason = "Brussels")

popdata%>%
  filter(Region=="Flanders") %>% 
  filter(DiffName) %>% 
  print(n=nrow(.))

popdata%>%
  filter(Region=="Wallonia") %>% 
  filter(DiffName) %>% 
  print(n=nrow(.))


### REASON 2
#Is it just the cities?
popdata %>%
  group_by(DiffName) %>% 
  summarise(mean=mean(population), median=median(population))


#Plotting average town size of small and larger towns
ggplot()+
  geom_histogram(data=popdata, aes(x=population), fill="grey", alpha=0.6)+
  geom_histogram(data=subset(popdata, DiffName==TRUE), aes(x=population), fill="cadetblue4", alpha=1)+
  scale_x_log10()+
  labs(y="Number of towns", title="Size of towns with two official names amongst all towns in Belgium")



#Check skew towards cities

#10% largest towns and cities in Belgium
quantile(popdata$population, probs = seq(from = 0, to = 1, by = .1))


popdata %>% 
  filter(population > 34000) %>%
  arrange(population) %>% 
  print(n=nrow(.))

popdata %>% 
  filter(population > 34000) %>%
  summarise(NTowns=n(), N_SameName=n()-sum(DiffName), N_DiffName=sum(DiffName), 
            Prop_SameName =1-round(mean(DiffName),2), Prop_DiffName=round(mean(DiffName),2))

reason_city <- popdata %>% 
  filter(population > 34000) %>%
  filter(Region != "Brussels agglomeration") %>% 
  filter(DiffName) %>% 
  mutate(Reason = "City")




#3. FACILITiIES AND GERMAN SPEAKING


#Duitstalig gebied (and adding two communities with facilities for german speaking)
germanspeaking <- c("Eupen", "Kelmis", "Lontzen", "Raeren", "Amel", "Büllingen", "Burg-Reuland", "Bütgenbach", 
                    "Sankt Vith", "Malmedy", "Weismes")

popdata %>% 
  filter(TownNL %in% germanspeaking) %>%
  summarise(NTowns=n(), N_SameName=n()-sum(DiffName), N_DiffName=sum(DiffName), 
            Prop_SameName =1-round(mean(DiffName),2), Prop_DiffName=round(mean(DiffName),2))

popdata %>% 
  filter(TownNL %in% germanspeaking) %>%
  filter(DiffName==TRUE) %>% 
  print(n=nrow(.))

reason_german <- popdata %>% 
  filter(TownNL %in% germanspeaking) %>%
  filter(DiffName) %>% 
  mutate(Reason = "German region")




#Facilities

faciliteiten <- c("Bever", "Drogenbos", "Herstappe", "Kraainem", "Linkebeek", "Mesen", "Ronse", 
                  "Sint-Genesius-Rode", "Spiere-Helkijn", "Voeren", "Wemmel", "Wezembeek-Oppem", 
                  "Edingen", "Komen-Waasten", "Moeskroen", "Vloesberg")


popdata %>% 
  filter(TownNL %in% faciliteiten) %>%
  summarise(NTowns=n(), N_SameName=n()-sum(DiffName), N_DiffName=sum(DiffName), 
            Prop_SameName =1-round(mean(DiffName),2), Prop_DiffName=round(mean(DiffName),2))

popdata %>% 
  filter(TownNL %in% faciliteiten) %>%
  filter(DiffName==TRUE) %>% 
  print(n=nrow(.))


reason_facilities <- popdata %>% 
  filter(TownNL %in% faciliteiten) %>%
  filter(DiffName) %>% 
  anti_join(reason_city) %>% 
  mutate(Reason = "Language facilities")




#Language border

language_border <- c("Heuvelland", "Komen-Waasten", "Mesen", "Menen", "Kortrijk", "Moeskroen", "Spiere-Helkijn",
                     "Ronse", "Elzele", "Vloesberg", "Lessen", "Geraardsbergen", "Bever", "Opzullik",
                     "Edingen", "Rebecq", "Tubeke", "Kasteelbrakel", "Halle", "Sint-Genesius-Rode", 
                     "Eigenbrakel", "Terhulpen", "Waver", "Graven", "Bevekom", "Geldenaken", "Tienen", "Lijsem", 
                     "Hannuit", "Borgworm", "Oerle", "Tongeren", "Bitsingen", "Voeren", "Wezet")


reason_langborder <- popdata %>% 
  filter(TownNL %in% language_border) %>%
  filter(DiffName) %>% 
  anti_join(reason_city) %>% 
  anti_join(reason_facilities) %>% 
  mutate(Reason = "Language border")


#Other
reason_other <- popdata %>% 
  filter(DiffName) %>% 
  anti_join(reason_city) %>% 
  anti_join(reason_BXL) %>% 
  anti_join(reason_german) %>% 
  anti_join(reason_facilities) %>% 
  anti_join(reason_langborder) %>%
  mutate(Reason = "Other")


#Merging reasons
reason <- bind_rows(reason_BXL, reason_city, reason_german, reason_facilities, reason_langborder, reason_other)

#Searching for duplicates before join
reason %>% 
  group_by(REFNIS) %>% 
  filter(n() > 1)


#Joining
popdata_reason <- left_join(popdata, reason)





#Where are we
popdata_reason %>%
  filter(DiffName)%>%
  group_by(Reason)%>%
  count()

popdata_reason %>%
  filter(DiffName)%>%
  filter(Reason=="Other")


##########
#MAPPING
##########

#creating a Region2 for plotting
popdatamap <- popdata %>%
  mutate(Region2 = ifelse(DiffName==TRUE, Region, NA))

#check popdatamap
popdatamap %>%
  group_by(Region2) %>% 
  summarise(n())


#Importing SPdataframe for Belgium
data("BE_ADMIN_MUNTY", package="BelgiumMaps.StatBel")
glimpse(BE_ADMIN_MUNTY@data, max.level=2)

#Merging my 2017 data with the SPdataframe
mapdata <- merge(BE_ADMIN_MUNTY, popdatamap, by.x = "CD_MUNTY_REFNIS", by.y = "REFNIS")
glimpse(mapdata@data, max.level=2)


#palette making
virpalette <- rev(viridis(3))
palette5 <- c(virpalette, "#E41A1C", "#FC8D62")
#infpalette <- inferno(3)
#magpalette <- magma(3)
#palette <- brewer.pal(3, "YlGnBu")

#Plot different regions
regionplot<- tm_shape(mapdata) +
  tm_fill(col="Region", palette=virpalette,
          title = "Regions in Belgium",
          popup.vars = c("TownNL","TownFR", "population"))+
  tm_polygons()+
  tm_layout(legend.position = c("left", "bottom"))


#Plot to show those with differnet name by region
nameplot <- tm_shape(mapdata) +
  tm_fill(col="Region2", palette=virpalette, 
          colorNA = "gray90", textNA="Same name", 
          title = "Different regional town names",legend.position = c("left", "bottom" ))+
  tm_polygons()+
  tm_layout(legend.position = c("left", "bottom"))

tmap_arrange(regionplot, nameplot)


###Reasonplot


#creating a Region2 for plotting
popdatamap_reason <- popdata_reason %>%
  mutate(Region2 = ifelse(DiffName==TRUE, Region, NA))

#check popdatamap
popdatamap_reason %>%
  group_by(Region2) %>% 
  summarise(n())



#Merging my 2017 data with the SPdataframe
mapdata_reason <- merge(BE_ADMIN_MUNTY, popdatamap_reason, by.x = "CD_MUNTY_REFNIS", by.y = "REFNIS")




#reasonplot

palette5 <- c(virpalette, "#E41A1C", "#FC8D62")
palette6 <- c(virpalette, "#E41A1C", "#FC8D62", "#6C8790")
reason_map <- tm_shape(mapdata_reason) +
  tm_fill(col="Reason", palette=palette6, id="TownNL", 
          colorNA = "gray90", textNA="Same name", 
          title = "Different regional town names",legend.position = c("left", "bottom" ),
          popup.vars = c("TownNL","TownFR", "population", "Reason"))+
  tm_polygons()+
  tm_layout(legend.position = c("left", "bottom"))

#tmap_leaflet(reason_map)

#Building interactive plot

#Plot to show those with differnet name by region
int_plot <- tm_shape(mapdata_reason) +
  tm_fill(col="Region2", palette=virpalette, id="TownNL", 
          colorNA = "gray90", textNA="Same name", 
          title = "Different regional town names",
          popup.vars = c("TownNL","TownFR", "population", "Reason"))+
  tm_polygons()+
  tm_layout(legend.position = c("left", "bottom"))



library(leaflet)
tmap_leaflet(int_plot)

widget <- tmap_leaflet(int_plot)

library(htmlwidgets)
saveWidget(widget, "interactivePlot.html")





#searching for towns with substring
filter(popdata, grepl("mont", TownFR))