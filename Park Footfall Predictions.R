setwd("C:\\Carlson MSBA\\AV")
a = read.csv("Train_xyqdbho.csv")
train = read.csv("Train_xyqdbho.csv")

install.packages(Hmisc)
install.packages(mice)
library(Hmisc)
library(mice)

describe(train)
summary(train)

install.packages("DataCombine")
library(DataCombine)

library(dplyr)
library("VIM")

aggr(train, prop=FALSE, numbers=TRUE)
matrixplot(train)

library(dplyr)
train = tbl_df(train)
lapply(train, class)
lapply(train, typeof)

imp.median <- function (a){
  missing <- is.na(a)  #This will be a logical vector
  imputed <- a
  imputed[missing] <- median(a,  na.rm=TRUE)
  return (imputed)
}

random.imp <- function (a){
  missing <- is.na(a) #this isolates the rows of the vector that has missing values
  n.missing <- sum(missing) #gives the number of missing values
  a.obs <- a[!missing] #this gets the empirical distribution - Not Null values 
  
  imputed <- a #temp placeholder to get the imputed values
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE) #impute using empirical dist.
  # make sure its with replacement
  # Doubt - What is Emperical distribution
  # x is a vector of elements =, from whihc we chose
  # n is the number of items to chose
  # Doubt - replacement = true meaning
  return (imputed)
}  

train$Max_Ambient_Pollution = random.imp(train$Max_Ambient_Pollution)

train[is.na(train$Average_Atmospheric_Pressure),]
train$Min_Atmospheric_Pressure = random.imp(train$Min_Atmospheric_Pressure)
train$Max_Atmospheric_Pressure = random.imp(train$Max_Atmospheric_Pressure)
train$Max_Ambient_Pollution = random.imp(train$Max_Ambient_Pollution)
train$Min_Ambient_Pollution = random.imp(train$Min_Ambient_Pollution)
train$Average_Atmospheric_Pressure = random.imp(train$Average_Atmospheric_Pressure)
describe(train)

train$Average_Breeze_Speed = random.imp(train$Average_Breeze_Speed)
train$Min_Breeze_Speed = random.imp(train$Min_Breeze_Speed)

colnames(train)

train$Min_Moisture_In_Park = imp.median(train$Min_Moisture_In_Park)
train$Max_Moisture_In_Park = imp.median(train$Max_Moisture_In_Park)
train$Average_Moisture_In_Park = imp.median(train$Average_Moisture_In_Park)

train$Date = a$Date
class(train$Date)

order(train, decreasing = TRUE)
train = train[order(as.Date(train$Date, format="%Y/%m/%d")),]
train$t = (1:114539)

colnames(train)

reg1<-lm(Footfall ~ t + Direction_Of_Wind + Average_Breeze_Speed + Max_Breeze_Speed + Min_Breeze_Speed + Var1 + Average_Atmospheric_Pressure + Max_Atmospheric_Pressure + Min_Atmospheric_Pressure + Min_Ambient_Pollution + Max_Ambient_Pollution + Average_Moisture_In_Park + Max_Moisture_In_Park + Min_Moisture_In_Park +  Location_Type, train)
summary(reg1)

#rmse
reg1.rmse <- sqrt(mean((reg1$residuals)^2))
reg1.rmse
print(acf(train$Footfall))

reg1<-lm(Footfall ~ t + Direction_Of_Wind + Average_Breeze_Speed + Max_Breeze_Speed + Min_Breeze_Speed + Var1 + Average_Atmospheric_Pressure + Max_Atmospheric_Pressure + Min_Atmospheric_Pressure + Min_Ambient_Pollution + Max_Ambient_Pollution + Average_Moisture_In_Park + Max_Moisture_In_Park + Min_Moisture_In_Park +  Location_Type, train)
summary(reg1)

#rmse
reg1.rmse <- sqrt(mean((reg1$residuals)^2))
reg1.rmse
print(acf(train$Footfall))

plot(train$t[1:5000], train$Footfall[1:5000], type = 'l')

train$tsquare = (train$t)^2

train$Month = Month(train$Date)

library(tidyr)
library(gridExtra)
train = separate(train, Date, c("Day", "Month", "Year"), remove = FALSE)

train$Month = factor(train$Month)
train$Day = factor(train$Day)
train$Year = factor(train$Year)

reg1<-lm(Footfall[1:114538] ~  Year[1:114538] + Day[1:114538] + Month[1:114538] + Footfall[2:114539] + Direction_Of_Wind[1:114538] + Average_Breeze_Speed[1:114538] + Max_Breeze_Speed[1:114538] + Min_Breeze_Speed[1:114538] + Var1[1:114538] + Average_Atmospheric_Pressure[1:114538] + Max_Atmospheric_Pressure[1:114538] + Min_Atmospheric_Pressure[1:114538] + Min_Ambient_Pollution[1:114538] + Max_Ambient_Pollution[1:114538] + Average_Moisture_In_Park[1:114538] + Max_Moisture_In_Park[1:114538] + Min_Moisture_In_Park[1:114538] +  Location_Type[1:114538], train)
summary(reg1)

#rmse
reg1.rmse <- sqrt(mean((reg1$residuals)^2))
reg1.rmse
print(pacf(train$Footfall))
train2 = train[1:114538,]

test = read.csv("Test_pyI9Owa.csv")
predTrn <- predict(reg1, newdata=train2)
predTst <- predict(reg1, newdata=test)

test
test$Date <- gsub('/', '-', test$Date)
