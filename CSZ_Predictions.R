# CSZ PROJECT
# Ultra-short-term forecasting of Total Load or something else


# https://stackoverflow.com/questions/43880823/subset-dataframe-based-on-posixct-date-and-time-greater-than-datetime-using-dply
# https://stackoverflow.com/questions/38396516/dplyr-filter-based-on-another-column
# https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
# https://www.r-bloggers.com/how-to-filter-in-r-a-detailed-introduction-to-the-dplyr-filter-function/
# https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
# https://www.guru99.com/r-decision-trees.html


library(readr)
library(MASS)
library(e1071)
library(rpart)
library(randomForest)
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
Tree_Anova <- rpart(Load_Now ~ Load_Min15 + Load_Day1B, Temp, minsplit = 1, minbucket = 1, cp=0.000001)
Tree_Anova_H <- rpart(Load_Now ~ Load_Min15 + Load_Day1B + Hour, Temp, minsplit = 1, minbucket = 1, cp=0.000001)

# printcp(Tree)
# plotcp(Tree)
# rpart.plot(Tree, type=1, extra=1)

# --- Random Forest --- #                                                                     # Sprawdzic z godzinami - H
rf <- randomForest(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, ntree = 10)               # -------------------------
rf100 <- randomForest(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, ntree = 100)           # -------------------------
# rf.pred <- predict(rf, newdata = newdata)                                                   # -------------------------
newdata$RFpred <- predict(rf, newdata = newdata)                                              # -------------------------
newdata$RF100pred <- predict(rf100, newdata = newdata)                                        # -------------------------

# --- SVM --- #                                                                                                           # Zrobic SVM
SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial" , cost = 1, gamma = 2, scale = F)        # ----------
newdata$SVM <- predict(SVM_Data, newdata[,4:5])                                                                           # ----------



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

colnames(newdata) <- c("Time", "Load_Now", "Hour", "Load_Min15", "Load_Day1B", "Class", "ClassPredict", "ClassPre_Shift")
# newdata[,4:5]


# --- Predykcja na nowym roku --- #
newdata$ClassPredict <- predict(Tree, newdata[,4:5], type = "class")
newdata$ClassPredictAnova <- predict(Tree_Anova, newdata[,4:5])
newdata$ClassPredictAnovaH <- predict(Tree_Anova_H, newdata[,3:5])


# --- Shift predykcji o jedno w górę -> lepsza korelacja --- #
newdata$ClassPre_Shift <- c(as.numeric(as.character(newdata$ClassPredict[2:length(newdata$ClassPredict)])), as.numeric(as.character(newdata$ClassPredict[length(newdata$ClassPredict)])))
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
newdata$ClassPreAnovaH_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnovaH[2:length(newdata$ClassPredictAnovaH)])), as.numeric(as.character(newdata$ClassPredictAnovaH[length(newdata$ClassPredictAnovaH)])))
newdata$RFPre_Shift <- c(as.numeric(as.character(newdata$RFpred[2:length(newdata$RFpred)])), as.numeric(as.character(newdata$RFpred[length(newdata$RFpred)])))
newdata$RF100Pre_Shift <- c(as.numeric(as.character(newdata$RF100pred[2:length(newdata$RF100pred)])), as.numeric(as.character(newdata$RF100pred[length(newdata$RF100pred)])))


# --- MAL i MAE -> sprawdzenie odchylenia prognozy --- #
MAE <- sum(abs(newdata$Load_Now - newdata$ClassPre_Shift)) / length(newdata$Load_Now)
MAPE <- sum(abs(newdata$Load_Now - newdata$ClassPre_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

MAE_Anova <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

MAE_AnovaH <- sum(abs(newdata$Load_Now - newdata$ClassPreAnovaH_Shift)) / length(newdata$Load_Now)
MAPE_AnovaH <- sum(abs(newdata$Load_Now - newdata$ClassPreAnovaH_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

MAE_RF <- sum(abs(newdata$Load_Now - newdata$RFPre_Shift)) / length(newdata$Load_Now)
MAPE_RF <- sum(abs(newdata$Load_Now - newdata$RFPre_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

MAE_RF100 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_Shift)) / length(newdata$Load_Now)
MAPE_RF100 <- sum(abs(newdata$Load_Now - newdata$RF100Pre_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


# --- Prognoza Naiwna --- #
Naiwna <- HU_Data %>% filter(utc_timestamp > (rok_start2) & utc_timestamp < end2)
Naiwna$godzina <- hour(Naiwna$utc_timestamp)
Naiwna$Predict <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(15)) & utc_timestamp < end2-minutes(15)) %>% select(HU_load_actual_entsoe_transparency)
Naiwna$Predict <- as.matrix(Naiwna$Predict)
colnames(Naiwna) <- c("Time", "Load_Now", "Hour", "Predict")

MAE_Naiwna <- sum(abs(Naiwna$Load_Now - Naiwna$Predict)) / length(Naiwna$Load_Now)
MAPE_Naiwna <- sum(abs(Naiwna$Load_Now - Naiwna$Predict) / Naiwna$Load_Now) / length(Naiwna$Load_Now) * 100




  
  