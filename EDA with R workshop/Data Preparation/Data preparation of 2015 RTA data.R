
#### Data Preparation of 2015 Road Traffic Accident ###

library(dplyr)

# Read data in
setwd("C:\\Users\\darsh\\OneDrive - Elastacloud Limited\\Meetups\\IWDS\\session 8- EDA\\Data\\2015")
Accidents <- read.csv("Accidents_2015.csv")
Casualties <- read.csv("Casualties_2015.csv")
Vehicals <- read.csv("Vehicles_2015.csv")
CarModels <- read.csv("2015_Make_Model.csv")

names(Accidents) #  32
names(Casualties) # 16
names(Vehicals) # 23 # very similar to car models so just ignore!
names(CarModels) # 24

# subset by intuition
Accidents <- Accidents[,c(1,4,5,7,8,9,10,11,12,13,17,18,25,26,27,30,31)] # 17
Casualties <- Casualties[,c(1,5,6,8,14)] # 5
CarModels <- CarModels[,c(1,4,9,16,20,23,24)] #  23

#initial inspection of datasets- subset further if necessary
summary(Accidents)
summary(Casualties)
summary(CarModels)

# check uniqueness of keys to join by
length(unique(Accidents$Accident_Index)) #140056
length(unique(Casualties$Accident_Index)) #140056 --> this data set has duplicated keys, for simplicity make unique by taking the first instance of each Accident index
length(unique(CarModels$ï..Accident_Index)) #92129 --> this data set has duplicated keys, for simplicity make unique by taking the first instance of each Accident index

# make Casualties unique by taking the first instance of each Accident index
Casualties <- Casualties[!rev(duplicated(rev(Casualties$Accident_Index))),]
# make CarModels unique by taking the first instance of each Accident index
CarModels <- CarModels[!rev(duplicated(rev(CarModels$ï..Accident_Index))),]

# join data sets
Accidents2015 <- inner_join(Accidents, Casualties, by = "Accident_Index")
Accidents2015 <- left_join(Accidents2015, CarModels, by = c("Accident_Index" = "ï..Accident_Index")) # 27

summary(Accidents2015)

write.csv(Accidents2015, "Accidents2015.csv",row.names = FASLE)
