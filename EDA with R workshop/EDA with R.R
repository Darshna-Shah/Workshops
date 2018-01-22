
##########################################################################################################
# IWDS session 8: EDA with R (missing values & imputation, outlier analyses, correlation matrices, ggplot)
##########################################################################################################
library(lubridate)
library(mice)
library(VIM)
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)

#Import Data
setwd("C:\\Users\\darsh\\OneDrive - Elastacloud Limited\\Meetups\\IWDS\\session 8- EDA\\Data")
Accidents2016 <- read.csv("Accidents2016.csv")

dim(Accidents2016) #[1] 136621     27
names(Accidents2016) # quickly see the names of all variables in a data set, check the names are logical, can also be used to easily subset data
str(Accidents2016) # used to check data types 
summary(Accidents2016) # used to correct data types, check missing values, outliers (difference between mean and max), eyeball correlations and get a guide on the distribution of each variable in a data set

# correct data types
Accidents2016$Date <- as.Date(Accidents2016$Date, format = "%d/%m/%Y")
Accidents2016$Time <- as.character(Accidents2016$Time)
Accidents2016$Time <- hm(Accidents2016$Time)


##########################################################################################################

#######################
#Examine missing values
#######################
summary(Accidents2016) # variables with missing data: -Longitude(7) - Latitude(7) -Time(2) -Spped_Limit(37) -make(30306) -model(30306)
levels(Accidents2016$make)
levels(Accidents2016$model) # 16000+ levels insight may be minimal so drop

#Many machine learning algorithms do not like missing values so they need to be removed or imputed. 
#There are many different ways to impute data, but before doing so we need to check only 10-20% of obsverations are missing per variable we wish to impute.Anymore than this threshold and we should consider removing the variable from analysis.
Accidents2016_num <- Accidents2016[,c(-1,-7,-9,-26,-27)] # consider only number value variables
summary(Accidents2016_num)
# convert NULL to NA
Accidents2016_num[Accidents2016_num == "NULL"] <- NA

#######################
#Impute missing values
######################
Accident2016_impute <- mice(Accidents2016_num, m=5, maxit = 10, method = 'pmm', seed = 500)
Accident2016_impute_complete <- mice::complete(Accident2016_impute, 2)
summary(Accident2016_impute_complete)

Accidents2016_Final <- cbind(Accident2016_impute_complete, Accidents2016[,c(7,9,26,1)])
summary(Accidents2016_Final)

##########################################################################################################

#################################################################################
#Examine outliers with summary (focus only on truely numeric variables for today)
#################################################################################

# There are many ways to define outliers and consequently many ways to deal with them. (see pdf/powerpoint for useful links)
summary(Accidents2016_Final) # variable with potential outliers: Number of casualties, age of casualty

#scatter plots
ggplot(Accidents2016_Final, aes(x= Date, y= Number_of_Casualties)) + geom_point()
ggplot(Accidents2016_Final, aes(x= Date, y= Age_of_Casualty)) + geom_point()

# boxplot
ggplot(Accidents2016_Final, aes(x= Date, y= Number_of_Casualties)) + geom_boxplot()
ggplot(Accidents2016_Final, aes(x= Date, y= Age_of_Casualty)) + geom_boxplot()

#subset data set to remove outliers
Accidents2016_Final <- subset(Accidents2016_Final, Accidents2016_Final$Number_of_Casualties < 30)

##########################################################################################################
# Examine correlations between reslationships to formulate hypothses

# to examine correlations all variables must be numeric- make Accident2016_impute_complete variables numeric
Accident2016_impute_complete1 <- data.frame(lapply(Accident2016_impute_complete, function(x) as.numeric(as.character(x))))
#rename variable to make correlation plot sizeable
names(Accident2016_impute_complete1)[names(Accident2016_impute_complete1)== "Did_Police_Officer_Attend_Scene_of_Accident"] <- "Police_Attend"

cor(Accident2016_impute_complete1)

# 3 different ways to view corelation matrix
corrplot(cor(Accident2016_impute_complete1), tl.cex = 0.75, tl.col = "black")

corrplot(cor(Accident2016_impute_complete1), method= "number",tl.cex = 0.75, tl.col = "black")

corrplot(cor(Accident2016_impute_complete1), order="hclust",
         col=brewer.pal(n=8, name="PuOr"),tl.cex = 0.75, tl.col = "black")

# Other plots and aggregations

#convert severity variable to factor
Accidents2016_Final$Accident_Severity <- as.factor(Accidents2016_Final$Accident_Severity)


#summarise the number of vehicles involved in different severity accidents
Severity_NoVehicles <- Accidents2016_Final %>% group_by(Accident_Severity) %>% summarise(Avg_Vehicles = mean(Number_of_Vehicles))
# plot aggregation
ggplot(Severity_NoVehicles, aes(x = Accident_Severity, y = Avg_Vehicles)) +
  geom_bar(stat = "identity") +
  ggtitle("Average number of vehicles involved in different severity accidents")

# Number of vehicles involved per severity category over time
Vehicles_date <- Accidents2016_Final %>% group_by(Accident_Severity, Date) %>% summarise(Avg_vehicles = mean(Number_of_Vehicles))
#plot aggregation
ggplot(Vehicles_date, aes(x = Date, y = Avg_vehicles, group = Accident_Severity, colour = Accident_Severity)) +
  geom_line() +
  ggtitle("Number of vehicles involved per severity category over time")

# summarise number of casualities involved in different severity accidents
Severity_casualities <- Accidents2016_Final %>% group_by(Accident_Severity) %>% summarise(Avg_casualties = mean(Number_of_Casualties))
#plot aggregation
ggplot(Severity_casualities, aes(x = Accident_Severity, y = Avg_casualties)) +
  geom_bar(stat = "identity") +
  ggtitle("Average number of casualties per severity accident category")


