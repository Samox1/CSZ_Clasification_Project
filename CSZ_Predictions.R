# CSZ PROJECT
# Ultra-short-term forecasting of Total Load or something else


# https://stackoverflow.com/questions/43880823/subset-dataframe-based-on-posixct-date-and-time-greater-than-datetime-using-dply
# https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
# https://www.r-bloggers.com/how-to-filter-in-r-a-detailed-introduction-to-the-dplyr-filter-function/
# https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
# https://www.guru99.com/r-decision-trees.html


library(readr)
library(MASS)
library(e1071)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)

rm(list = ls())

HU_Data_ALL <- as.data.frame(read.table("HU_TS_15_min.csv",header=TRUE,sep=","))
HU_Data_ALL$utc_timestamp <- as.data.frame(ymd_hms(HU_Data_ALL$utc_timestamp, tz = "UTC"))

start1 <- lubridate::ymd_hms('2014-12-22 23:45:00', tz = "UTC")
rok_start1 <- lubridate::ymd_hms('2017-04-30 22:45:00', tz = "UTC")
end1 <- lubridate::ymd_hms('2018-04-30 23:00:00', tz = "UTC")


rok_start2 <- lubridate::ymd_hms('2018-04-30 22:45:00', tz = "UTC")
end2 <- lubridate::ymd_hms('2019-04-30 23:00:00', tz = "UTC")

HU_Data <- HU_Data_ALL %>% filter(utc_timestamp[[1]] > rok_start1-days(2) & utc_timestamp[[1]] < end2+minutes(15))
colnames(HU_Data[,1]) <- ""
HU_Data <- HU_Data[, c("utc_timestamp" , "HU_load_actual_entsoe_transparency")]

NA_sum <- sum(is.na(HU_Data$HU_load_actual_entsoe_transparency))
levele <- as.integer(max(HU_Data$HU_load_actual_entsoe_transparency) - min(HU_Data$HU_load_actual_entsoe_transparency))
minMW <- min(HU_Data$HU_load_actual_entsoe_transparency)
maxMW <- max(HU_Data$HU_load_actual_entsoe_transparency)


Daty_Prognozowane <- seq(rok_start1, end1, by = '15 min')

# --- Utworzenie nowej tablicy pod drzewo --- #
Temp <- HU_Data %>% filter(utc_timestamp > (rok_start1) & utc_timestamp < end1)
Temp$godzina <- hour(Temp$utc_timestamp)

Min15Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(15)) & utc_timestamp < end1-minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Min15B <- cbind(as.matrix(Min15Before))
rm(Min15Before)

Day1Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)) & utc_timestamp < end1-days(1)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Day1B <- cbind(as.matrix(Day1Before))
rm(Day1Before)

# Temp$Class <- as.integer(Temp$HU_load_actual_entsoe_transparency - minMW)
# Temp$Class <- Temp$HU_load_actual_entsoe_transparency
# Temp$Class <- factor(Temp$Class)
Temp$Class <- factor(as.integer(Temp$HU_load_actual_entsoe_transparency))

colnames(Temp) <- c("Time", "Load_Now", "Hour", "Load_Min15", "Load_Day1B", "Class")


# --- Testy na Drzewie --- #
Tree <- rpart(Class ~ Load_Min15 + Load_Day1B, Temp, method = "class", minsplit = 1, minbucket = 1, cp=0.000001)
# printcp(Tree)
# plotcp(Tree)
# rpart.plot(Tree, type=1, extra=1)



# --- Nowe dane --- #
newdata <- HU_Data %>% filter(utc_timestamp > (rok_start2) & utc_timestamp < end2)
newdata$godzina <- hour(newdata$utc_timestamp)

Min15Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(15)) & utc_timestamp < end2-minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
newdata$Min15B <- cbind(as.matrix(Min15Before))
rm(Min15Before)

Day1Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)) & utc_timestamp < end2-days(1)) %>% select(HU_load_actual_entsoe_transparency)
newdata$Day1B <- cbind(as.matrix(Day1Before))
newdata$Day1B <- as.matrix(newdata$Day1B)
rm(Day1Before)

# newdata$Class <- as.integer(newdata$HU_load_actual_entsoe_transparency - minMW)
# newdata$Class <- newdata$HU_load_actual_entsoe_transparency
# newdata$Class <- as.factor(newdata$Class)
newdata$Class <- factor(as.integer(newdata$HU_load_actual_entsoe_transparency))

colnames(newdata) <- c("Time", "Load_Now", "Hour", "Load_Min15", "Load_Day1B", "Class")
# newdata[,4:5]


# --- Predykcja na nowym roku --- #
newdata$ClassPredict <- predict(Tree, newdata[,4:5], type = "class")

# --- Shift predykcji o jedno w górę -> lepsza korelacja --- #
newdata$ClassPre_Shift <- c(as.numeric(as.character(newdata$ClassPredict[2:length(newdata$ClassPredict)])), as.numeric(as.character(newdata$ClassPredict[length(newdata$ClassPredict)])))

# --- MAL i MAE -> sprawdzenie odchylenia prognozy --- #
MAL <- sum(abs(newdata$Load_Now - newdata$ClassPre_Shift))
MAE <- sum(abs(newdata$Load_Now - newdata$ClassPre_Shift) / newdata$Load_Now)

