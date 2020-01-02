# CSZ PROJECT

library(readr)
library(MASS)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)

rm(list = ls())

HU_Data <- as.data.frame(read.table("HU_TS_15_min.csv",header=TRUE,sep=","))

daty_UTC <- lubridate::as_datetime(HU_Data$utc_timestamp, tz = "UTC")

# string1 <- as.character(HU_Data$utc_timestamp)
# daty_UTC <- lubridate::as_datetime(string1, tz = "UTC")
# View(daty_UTC)

# godz <- as.numeric(lubridate::hour(daty_UTC[50]))



