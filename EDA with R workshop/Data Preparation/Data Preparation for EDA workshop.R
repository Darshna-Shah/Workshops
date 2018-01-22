
# Data preparation for Exploratory Data Analysis workshop
library(dplyr)

# Read data in
setwd("C:\\Users\\darsh\\OneDrive - Elastacloud Limited\\Meetups\\IWDS\\session 8- EDA\\Data")
Accidents <- read.csv("dftRoadSafety_Accidents_2016.csv")
Casualties <- read.csv("Casualties.csv")
Vehicals <- read.csv("Vehicals.csv")
CarModels <- read.csv("MakeModel2016.csv")

names(Accidents) # 136621 x 32
names(Casualties) # 181384 x 16
names(Vehicals) # 252500 x 23 # very similar to car models so just ignore!
names(CarModels) # 252500 x 24

# subset by intuition
Accidents <- Accidents[,c(1,4,5,7,8,9,10,11,12,13,17,18,25,26,27,30,31)] # 136621 x 17
Casualties <- Casualties[,c(1,5,6,8,14)] # 181384 x 5
CarModels <- CarModels[,c(1,4,9,16,20,23,24)] # 252500 x 23

#initial inspection of datasets- subset further if necessary
summary(Accidents)
summary(Casualties)
summary(CarModels)

# check uniqueness of keys to join by
length(unique(Accidents$Accident_Index)) #136621
length(unique(Casualties$Accident_Index)) #136621 --> this data set has duplicated keys, for simplicity make unique by taking the first instance of each Accident index
length(unique(CarModels$ï..Accident_Index)) #136621 --> this data set has duplicated keys, for simplicity make unique by taking the first instance of each Accident index

# make Casualties unique by taking the first instance of each Accident index
Casualties <- Casualties[!rev(duplicated(rev(Casualties$Accident_Index))),]
# make CarModels unique by taking the first instance of each Accident index
CarModels <- CarModels[!rev(duplicated(rev(CarModels$ï..Accident_Index))),]

# join data sets
Accidents2016 <- inner_join(Accidents, Casualties, by = "Accident_Index")
Accidents2016 <- left_join(Accidents2016, CarModels, by = c("Accident_Index" = "ï..Accident_Index")) #136621 x 27

summary(Accidents2016)

write.csv(Accidents2016, "Accidents2016.csv")
