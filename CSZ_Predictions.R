# CSZ PROJECT

library(readr)
library(MASS)
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
end1 <- lubridate::ymd_hms('2019-04-30 23:00:00', tz = "UTC")

index_start <- which(HU_Data_ALL$utc_timestamp == start1)

HU_Data <- HU_Data_ALL %>% filter(utc_timestamp == as.POSIXct(start1))
HU_Data <- daty_UTC %>% detect(datetime == as.POSIXct(start1))

# HU_Data <- HU_Data_ALL[,which()]

# https://stackoverflow.com/questions/43880823/subset-dataframe-based-on-posixct-date-and-time-greater-than-datetime-using-dply
# https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
# https://www.r-bloggers.com/how-to-filter-in-r-a-detailed-introduction-to-the-dplyr-filter-function/