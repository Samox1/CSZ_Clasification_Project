# CSZ PROJECT
# Ultra-short-term forecasting of Total Load or something else


# https://stackoverflow.com/questions/43880823/subset-dataframe-based-on-posixct-date-and-time-greater-than-datetime-using-dply
# https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
# https://www.r-bloggers.com/how-to-filter-in-r-a-detailed-introduction-to-the-dplyr-filter-function/
# https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e

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
# daty_UTC <- data.frame(datetime = ymd_hms(HU_Data_ALL$utc_timestamp, tz = "UTC"))
# string1 <- as.character(HU_Data$utc_timestamp)
# daty_UTC <- lubridate::as_datetime(string1, tz = "UTC")
# View(daty_UTC)
# godz <- as.numeric(lubridate::hour(daty_UTC[50]))

start1 <- lubridate::ymd_hms('2014-12-22 23:45:00', tz = "UTC")
rok_start <- lubridate::ymd_hms('2018-04-30 22:45:00', tz = "UTC")
end1 <- lubridate::ymd_hms('2019-04-30 23:00:00', tz = "UTC")

# HU_Data <- HU_Data_ALL %>% filter(utc_timestamp[[1]] == start1)   # Working

HU_Data <- HU_Data_ALL %>% filter(utc_timestamp[[1]] > rok_start-days(2) & utc_timestamp[[1]] < end1+minutes(15))
colnames(HU_Data[,1]) <- ""
HU_Data <- HU_Data[, c("utc_timestamp" , "HU_load_actual_entsoe_transparency")]

NA_sum <- sum(is.na(HU_Data$HU_load_actual_entsoe_transparency))
levele <- as.integer(max(HU_Data$HU_load_actual_entsoe_transparency) - min(HU_Data$HU_load_actual_entsoe_transparency))
minMW <- min(HU_Data$HU_load_actual_entsoe_transparency)

# --- Test na pojedynczej dacie --- #
# dowolna_data <- lubridate::ymd_hms('2018-07-30 15:00:00', tz = "UTC")
# Temp <- HU_Data %>% filter(utc_timestamp == dowolna_data)
# godzina <- HU_Data %>% filter(utc_timestamp == dowolna_data) %>% select(utc_timestamp)
# Temp$godzina <- as.numeric(hour(ymd_hms(godzina[[1]], tz = "UTC")))
# Temp$Min15 <- HU_Data %>% filter(utc_timestamp == (dowolna_data - minutes(15))) %>% select(HU_load_actual_entsoe_transparency)
# Temp$Day_Before <- HU_Data %>% filter(utc_timestamp == (dowolna_data - days(1))) %>% select(HU_load_actual_entsoe_transparency)
# Temp$Class <- as.integer(Temp$Now - min(HU_Data$HU_load_actual_entsoe_transparency))

# for (x in Daty_Prognozowane){
# Temp <- HU_Data %>% filter(utc_timestamp == x)
# Temp$utc_timestamp <- rbind(Temp$utc_timestamp, (HU_Data %>% filter(utc_timestamp == x) %>% select(utc_timestamp)))
# czas <- 0
# czas <- HU_Data %>% filter(utc_timestamp == x) %>% select(utc_timestamp)
# Temp$godzina <- rbind(Temp$godzina, hour(czas))
# Temp$Now <- rbind(Temp$Now, as.numeric(HU_Data %>% filter(utc_timestamp == x) %>% select(HU_load_actual_entsoe_transparency)))
# Temp$Min15 <- rbind(Temp$Min15, as.numeric(HU_Data %>% filter(utc_timestamp == (x - minutes(15))) %>% select(HU_load_actual_entsoe_transparency)))
# Temp$Day_Before <- rbind(Temp$Day_Before, as.numeric(HU_Data %>% filter(utc_timestamp == (x - days(1))) %>% select(HU_load_actual_entsoe_transparency)))
# now1 <- as.numeric(Temp$Now[length(Temp$Now)])
# Temp$Class <- rbind(Temp$Class, as.integer(now1 - minMW))
# show(as_datetime(x))
# show(Temp)
# }

# Temp$godzina <- hour(Temp$utc_timestamp[[1]])
# colnames(Temp) <- c("time", "Now", "Godzina", "Min15", "Day_Before")

Daty_Prognozowane <- seq(rok_start, end1, by = '15 min')

# --- Utworzenie nowej tablicy pod drzewo --- #
Temp <- HU_Data %>% filter(utc_timestamp > (rok_start) & utc_timestamp < end1)
Temp$godzina <- hour(Temp$utc_timestamp)

Min15Before <- HU_Data %>% filter(utc_timestamp > (rok_start-minutes(15)) & utc_timestamp < end1-minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Min15B <- cbind(as.matrix(Min15Before))
rm(Min15Before)

Day1Before <- HU_Data %>% filter(utc_timestamp > (rok_start-days(1)) & utc_timestamp < end1-days(1)) %>% select(HU_load_actual_entsoe_transparency)
Temp$Day1B <- cbind(as.matrix(Day1Before))
rm(Day1Before)

Temp$Class <- as.integer(Temp$HU_load_actual_entsoe_transparency - minMW)

colnames(Temp) <- c("Time", "Load_Now", "Hour", "Load_Min15", "Load_Day1B", "Class")



Tree <- rpart(Class ~ Load_Min15 + Load_Day1B, Temp, minsplit = 1, minbucket = 1, cp=0.005)
rpart.plot(Tree, type=1, extra=1)
plotcp(Tree)











