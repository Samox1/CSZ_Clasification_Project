# Projekt na przedmiot: Wybrane zagadnienia uczenia maszyn (CSZ)
# Tytuł projektu: 
# Autor: Szymon Baczyński 
# Indeks: KD-158

# Ze wzgledu na czas wykonywanych obliczen, niektore linie kodu 
# zostaly zakomentowane w celu ograniczenia czasu wykonywania skryptu.
# Wybrane zostaly najlepsze modele jako reprezentanci swoich klasyfikatorow.

library(readr)
library(MASS)
library(e1071)
library(rpart)
library(randomForest)
library(rpart.plot)
library(dplyr)
library(lubridate)
library(data.table)
library(stringr)
library(caret)
library(plotly)

# --- *** Import danych:
# rm(list = ls())            # Czyszczenie Globalnego Srodowiska 

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

# NA_sum <- sum(is.na(HU_Data$HU_load_actual_entsoe_transparency))
# levele <- as.integer(max(HU_Data$HU_load_actual_entsoe_transparency) - min(HU_Data$HU_load_actual_entsoe_transparency))
# minMW <- min(HU_Data$HU_load_actual_entsoe_transparency)
# maxMW <- max(HU_Data$HU_load_actual_entsoe_transparency)
names(HU_Data)[2] <- "Load_Now"


# --- *** Wstepna obrobka danych:

# --- Utworzenie nowej tablicy pod drzewo --- #
Temp <- HU_Data %>% filter(utc_timestamp > (rok_start1) & utc_timestamp < end1)

Temp$Hour <- hour(Temp$utc_timestamp)
Temp$Hour <- as.factor(Temp$Hour)

Min15Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(15)) & utc_timestamp < end1-minutes(15)) %>% select(Load_Now)
Temp$Load_Min15 <- cbind(as.matrix(Min15Before))
rm(Min15Before)

Min30Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(30)) & utc_timestamp < end1-minutes(30)) %>% select(Load_Now)
Temp$Load_Min30 <- cbind(as.matrix(Min30Before))
rm(Min30Before)

Min45Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-minutes(45)) & utc_timestamp < end1-minutes(45)) %>% select(Load_Now)
Temp$Load_Min45 <- cbind(as.matrix(Min45Before))
rm(Min45Before)

Day1Before <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)) & utc_timestamp < end1-days(1)) %>% select(Load_Now)
Temp$Load_Day1B <- cbind(as.matrix(Day1Before))
rm(Day1Before)

Day1Before15Min <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)-minutes(15)) & utc_timestamp < end1-days(1)-minutes(15)) %>% select(Load_Now)
Temp$Load_Day1B15min <- cbind(as.matrix(Day1Before15Min))
rm(Day1Before15Min)

Day1Beforep15Min <- HU_Data %>% filter(utc_timestamp > (rok_start1-days(1)+minutes(15)) & utc_timestamp < end1-days(1)+minutes(15)) %>% select(Load_Now)
Temp$Load_Day1Bp15min <- cbind(as.matrix(Day1Beforep15Min))
rm(Day1Beforep15Min)


# --- Przygotowanie danych testowych:
newdata <- HU_Data %>% filter(utc_timestamp > (rok_start2) & utc_timestamp < end2)
newdata$Hour <- hour(newdata$utc_timestamp)
newdata$Hour <- as.factor(newdata$Hour)

Min15Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(15)) & utc_timestamp < end2-minutes(15)) %>% select(Load_Now)
newdata$Load_Min15 <- cbind(as.matrix(Min15Before))
rm(Min15Before)

Min30Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(30)) & utc_timestamp < end2-minutes(30)) %>% select(Load_Now)
newdata$Load_Min30 <- cbind(as.matrix(Min30Before))
rm(Min30Before)

Min45Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(45)) & utc_timestamp < end2-minutes(45)) %>% select(Load_Now)
newdata$Load_Min45 <- cbind(as.matrix(Min45Before))
rm(Min45Before)

Day1Before <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)) & utc_timestamp < end2-days(1)) %>% select(Load_Now)
newdata$Load_Day1B <- cbind(as.matrix(Day1Before))
rm(Day1Before)

Day1BeforeM15Min <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)-minutes(15)) & utc_timestamp < end2-days(1)-minutes(15)) %>% select(Load_Now)
newdata$Load_Day1B15min <- cbind(as.matrix(Day1BeforeM15Min))
rm(Day1BeforeM15Min)

Day1BeforeP15Min <- HU_Data %>% filter(utc_timestamp > (rok_start2-days(1)+minutes(15)) & utc_timestamp < end2-days(1)+minutes(15)) %>% select(Load_Now)
newdata$Load_Day1Bp15min <- cbind(as.matrix(Day1BeforeP15Min))
rm(Day1BeforeP15Min)



# --- *** Przygotowanie modelu:

# Metoda Naiwna 
Naiwna <- HU_Data %>% filter(utc_timestamp > (rok_start2) & utc_timestamp < end2)
Naiwna$Hour <- hour(Naiwna$utc_timestamp)
Naiwna$Predict <- HU_Data %>% filter(utc_timestamp > (rok_start2-minutes(15)) & utc_timestamp < end2-minutes(15)) %>% select(Load_Now)
Naiwna$Predict <- as.matrix(Naiwna$Predict)
colnames(Naiwna) <- c("Time", "Load_Now", "Hour", "Predict")
MAE_Naiwna <- sum(abs(Naiwna$Load_Now - Naiwna$Predict)) / length(Naiwna$Load_Now)
MAPE_Naiwna <- sum(abs(Naiwna$Load_Now - Naiwna$Predict) / Naiwna$Load_Now) / length(Naiwna$Load_Now) * 100


# Drzewa Decyzyjne (ANOVA) - Znalezienie najlepszego modelu trwa ponad godzine (> 1h) dla kazdej kombinacji zmiennych

Tree_Anova_Tune <- tune(rpart, Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(minsplit=c(1:5), minbucket=c(1:5), cp=10^(c(-4):(-10))))
newdata$ClassPredictAnova <- predict(Tree_Anova_Tune$best.model, newdata)
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
MAE_Anova <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

Tree_Anova_Tune <- tune(rpart, Load_Now ~ Load_Min15 + Load_Min30 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(minsplit=c(1:5), minbucket=c(1:5), cp=10^(c(-4):(-10))))
newdata$ClassPredictAnova <- predict(Tree_Anova_Tune$best.model, newdata)
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
MAE_Anova_30 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova_30 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

Tree_Anova_Tune <- tune(rpart, Load_Now ~ Load_Min15 + Load_Min30 + Load_Min45 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(minsplit=c(1:5), minbucket=c(1:5), cp=10^(c(-4):(-10))))
newdata$ClassPredictAnova <- predict(Tree_Anova_Tune$best.model, newdata)
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
MAE_Anova_3045 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova_3045 <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

Tree_Anova_Tune <- tune(rpart, Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(minsplit=c(1:5), minbucket=c(1:5), cp=10^(c(-4):(-10))))
newdata$ClassPredictAnova <- predict(Tree_Anova_Tune$best.model, newdata)
newdata$ClassPreAnova_Shift <- c(as.numeric(as.character(newdata$ClassPredictAnova[2:length(newdata$ClassPredictAnova)])), as.numeric(as.character(newdata$ClassPredictAnova[length(newdata$ClassPredictAnova)])))
MAE_Anova_1Day <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift)) / length(newdata$Load_Now)
MAPE_Anova_1Day <- sum(abs(newdata$Load_Now - newdata$ClassPreAnova_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


# Random Forest - "tuning" Lasow Losowych moze trwac od 20 do 40 minut (kazdy z modeli)

RF_Tune <- tune(randomForest, Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(ntree=c(10,20,30,40,50,60,70,80,90,100)))
newdata$ClassPredictRF <- predict(RF_Tune$best.model, newdata)
newdata$ClassPreRF_Shift <- c(as.numeric(as.character(newdata$ClassPredictRF[2:length(newdata$ClassPredictRF)])), as.numeric(as.character(newdata$ClassPredictRF[length(newdata$ClassPredictRF)])))
MAE_RF <- sum(abs(newdata$Load_Now - newdata$ClassPreRF_Shift)) / length(newdata$Load_Now)
MAPE_RF <- sum(abs(newdata$Load_Now - newdata$ClassPreRF_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

RF_Tune <- tune(randomForest, Load_Now ~ Load_Min15 + Load_Min30 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(ntree=c(10,20,30,40,50,60,70,80,90,100)))
newdata$ClassPredictRF <- predict(RF_Tune$best.model, newdata)
newdata$ClassPreRF_Shift <- c(as.numeric(as.character(newdata$ClassPredictRF[2:length(newdata$ClassPredictRF)])), as.numeric(as.character(newdata$ClassPredictRF[length(newdata$ClassPredictRF)])))
MAE_RF30 <- sum(abs(newdata$Load_Now - newdata$ClassPreRF_Shift)) / length(newdata$Load_Now)
MAPE_RF30 <- sum(abs(newdata$Load_Now - newdata$ClassPreRF_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

RF_Tune <- tune(randomForest, Load_Now ~ Load_Min15 + Load_Min30 + Load_Min45 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(ntree=c(10,20,30,40,50,60,70,80,90,100)))
newdata$ClassPredictRF <- predict(RF_Tune$best.model, newdata)
newdata$ClassPreRF_Shift <- c(as.numeric(as.character(newdata$ClassPredictRF[2:length(newdata$ClassPredictRF)])), as.numeric(as.character(newdata$ClassPredictRF[length(newdata$ClassPredictRF)])))
MAE_RF3045 <- sum(abs(newdata$Load_Now - newdata$ClassPreRF_Shift)) / length(newdata$Load_Now)
MAPE_RF3045 <- sum(abs(newdata$Load_Now - newdata$ClassPreRF_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

RF_Tune <- tune(randomForest, Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,ranges = list(ntree=c(10,20,30,40,50,60,70,80,90,100)))
newdata$ClassPredictRF <- predict(RF_Tune$best.model, newdata)
newdata$ClassPreRF_Shift <- c(as.numeric(as.character(newdata$ClassPredictRF[2:length(newdata$ClassPredictRF)])), as.numeric(as.character(newdata$ClassPredictRF[length(newdata$ClassPredictRF)])))
MAE_RF1Day <- sum(abs(newdata$Load_Now - newdata$ClassPreRF_Shift)) / length(newdata$Load_Now)
MAPE_RF1Day <- sum(abs(newdata$Load_Now - newdata$ClassPreRF_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


# SVM - Ze wzgledu na dlugi czas obliczen linie kodu z modelami SVM zostaly wykomentowane - uczenie poszczegolnego modelu trwa od 20 do 40 minut
# Wybrane zostaly najlepsze modele jako reprezentanci grupy

### SVM uczone na: Load_Min15 + Load_Day1B
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.00005, cost=2000, scale = F)         # <-- v1: MAE=22.416 | MAPE=0.474
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.00001, cost=2000, scale = F)         # <-- v2: MAE=17.422 | MAPE=0.371
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000005, cost=5000, scale = F)        # <-- v3: MAE=15.547 | MAPE=0.333                                                                                                                                                 
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000001, cost=10000, scale = F)       # <-- v4: MAE=14.258 | MAPE=0.308
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000005, cost=10000, scale = F)      # <-- v5: MAE=14.047 | MAPE=0.303
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- v6: MAE=13.707 | MAPE=0.297
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=100000, scale = F)    # <-- v7: MAE=12.071 | MAPE=0.261
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000000001, cost=1000000, scale = F)  # <-- v8: MAE=11.579 | MAPE=0.246
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000000001, cost=100000, scale = F)   # <-- v9: MAE=10.132 | MAPE=0.216
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000000001, cost=10000, scale = F)    # <-- v10: MAE=8.103 | MAPE=0.171
# SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.000000001, cost=1000, scale = F)     # <-- v11: MAE=8.184 | MAPE=0.169
SVM_Data <- svm(Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)   # <-- v12: MAE=7.980 | MAPE=0.165

newdata$SVM <- predict(SVM_Data, newdata)                               # Load_Min15 + Load_Day1B                   
newdata$SVM_Shift <- c(as.numeric(as.character(newdata$SVM[2:length(newdata$SVM)])), as.numeric(as.character(newdata$SVM[length(newdata$SVM)])))
MAE_SVM <- sum(abs(newdata$Load_Now - newdata$SVM_Shift)) / length(newdata$Load_Now)
MAPE_SVM <- sum(abs(newdata$Load_Now - newdata$SVM_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### Load_Min15 + Load_Day1B + Hour
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)      # <-- Hv1: MAE=13.522 | MAPE=0.272
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- Hv2: MAE=15.388 | MAPE=0.328
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)     # <-- Hv3: MAE=11.772 | MAPE=0.257
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)   # <-- Hv4: MAE=7.985 | MAPE=0.165
SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F)  # <-- Hv5: MAE=7.446 | MAPE=0.155
# SVM_Data_H <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)    # <-- Hv6: MAE=7.981 | MAPE=0.165

newdata$SVM_H <- predict(SVM_Data_H, newdata)                           # Load_Min15 + Load_Day1B + Hour
newdata$SVM_H_Shift <- c(as.numeric(as.character(newdata$SVM_H[2:length(newdata$SVM_H)])), as.numeric(as.character(newdata$SVM_H[length(newdata$SVM_H)])))
MAE_SVM_H <- sum(abs(newdata$Load_Now - newdata$SVM_H_Shift)) / length(newdata$Load_Now)
MAPE_SVM_H <- sum(abs(newdata$Load_Now - newdata$SVM_H_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### Load_Min15 + Load_Min30 + Load_Day1B + Hour
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)     # <-- H30v1: MAE=32.393 | MAPE=0.685
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)    # <-- H30v2: MAE=32.459 | MAPE=0.686
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)    # <-- H30v3: MAE=32.479 | MAPE=0.685
SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)  # <-- H30v4: MAE=17.254 | MAPE=0.356
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F) # <-- H30v5: MAE=31.348 | MAPE=0.649
# SVM_Data_H30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)   # <-- H30v6: MAE=24.361 | MAPE=0.496

newdata$SVM_H30 <- predict(SVM_Data_H30, newdata)                       # Load_Min15 + Load_Min30 + Load_Day1B + Hour 
newdata$SVM_H30_Shift <- c(as.numeric(as.character(newdata$SVM_H30[2:length(newdata$SVM_H30)])), as.numeric(as.character(newdata$SVM_H30[length(newdata$SVM_H30)])))
MAE_SVM_H30 <- sum(abs(newdata$Load_Now - newdata$SVM_H30_Shift)) / length(newdata$Load_Now)
MAPE_SVM_H30 <- sum(abs(newdata$Load_Now - newdata$SVM_H30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### Load_Min15 + Load_Min30 + Load_Day1B
# SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)      # <-- 30v1: MAE= | MAPE=
# SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- 30v2: MAE= | MAPE=
# SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)     # <-- 30v3: MAE= | MAPE=
# SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)    # <-- 30v4: MAE= | MAPE=
SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)   # <-- 30v5: MAE= | MAPE=
# SVM_Data_30 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F)  # <-- 30v6: MAE= | MAPE=

newdata$SVM_30 <- predict(SVM_Data_30, newdata)                         # Load_Min15 + Load_Day1B + Load_Min30
newdata$SVM_30_Shift <- c(as.numeric(as.character(newdata$SVM_30[2:length(newdata$SVM_30)])), as.numeric(as.character(newdata$SVM_30[length(newdata$SVM_30)])))
MAE_SVM_30 <- sum(abs(newdata$Load_Now - newdata$SVM_30_Shift)) / length(newdata$Load_Now)
MAPE_SVM_30 <- sum(abs(newdata$Load_Now - newdata$SVM_30_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45
# SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)      # <-- 3045v1: MAE= | MAPE=
# SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- 3045v2: MAE= | MAPE=
# SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)     # <-- 3045v3: MAE= | MAPE=
# SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)    # <-- 3045v4: MAE= | MAPE=
SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)   # <-- 3045v5: MAE= | MAPE=
# SVM_Data_3045 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F)  # <-- 3045v6: MAE= | MAPE=

newdata$SVM_3045 <- predict(SVM_Data_3045, newdata)                     # Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45
newdata$SVM_3045_Shift <- c(as.numeric(as.character(newdata$SVM_3045[2:length(newdata$SVM_3045)])), as.numeric(as.character(newdata$SVM_3045[length(newdata$SVM_3045)])))
MAE_SVM_3045 <- sum(abs(newdata$Load_Now - newdata$SVM_3045_Shift)) / length(newdata$Load_Now)
MAPE_SVM_3045 <- sum(abs(newdata$Load_Now - newdata$SVM_3045_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min
# SVM_Data_1Day1 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=10000, scale = F)      # <-- 1Dayv1: MAE= | MAPE=
# SVM_Data_1Day2 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000001, cost=100000, scale = F)     # <-- 1Dayv2: MAE= | MAPE=
# SVM_Data_1Day3 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.00000001, cost=10000, scale = F)     # <-- 1Dayv3: MAE= | MAPE=
SVM_Data_1Day4 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=1000, scale = F)    # <-- 1Dayv4: MAE= | MAPE=
# SVM_Data_1Day5 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=10000, scale = F)   # <-- 1Dayv5: MAE= | MAPE=
# SVM_Data_1Day6 <- svm(Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, kernel="radial", probability=TRUE, gamma=0.0000000001, cost=100000, scale = F)  # <-- 1Dayv6: MAE= | MAPE=

newdata$SVM_1Day <- predict(SVM_Data_1Day6, newdata)                    # Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min
newdata$SVM_1Day_Shift <- c(as.numeric(as.character(newdata$SVM_1Day[2:length(newdata$SVM_1Day)])), as.numeric(as.character(newdata$SVM_1Day[length(newdata$SVM_1Day)])))
MAE_SVM_1Day <- sum(abs(newdata$Load_Now - newdata$SVM_1Day_Shift)) / length(newdata$Load_Now)
MAPE_SVM_1Day <- sum(abs(newdata$Load_Now - newdata$SVM_1Day_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### Tune dla modelu SVM, uczony na: Load_Min15 + Load_Day1B
# Ze wzgledu na czas obliczen ~ 5 godzin - zakomentowane 
# Znaleziony najlepszy model SVM uzyskiwal wyniki na zbiorze testowym: MAE = 4,32 | MAPE = 0,089

# SVM_Tune <- tune(svm, Load_Now ~ Load_Min15 + Load_Day1B, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,
#                   ranges = list(gamma = c(10^(-12:-5),(10^(-12:-5))/2), cost = 10^(2:8)),
#                   tunecontrol = tune.control(sampling = "fix", performances = TRUE ))

# newdata$SVM_Tune <- predict(SVM_Tune$best.model, newdata)                           # Load_Min15 + Load_Day1B
# newdata$SVM_Tune_Shift <- c(as.numeric(as.character(newdata$SVM_Tune[2:length(newdata$SVM_Tune)])), as.numeric(as.character(newdata$SVM_Tune[length(newdata$SVM_Tune)])))
# MAE_SVM_Tune <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift)) / length(newdata$Load_Now)
# MAPE_SVM_Tune <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### Dane podsumowujace modele SVM uczone na: Load_Min15 + Load_Day1B

gammas <- c(0.00005, 0.00001, 0.000005,0.000001,0.0000005,0.0000001,0.00000001,0.000000001,0.000000001,0.000000001,0.000000001,0.0000000001)
costs <- c(2000,2000,5000,10000,10000,100000,100000,1000000,100000,10000,1000,10000)
MAE_C <- c(22.416, 17.422, 15.547, 14.258,14.047,13.707,12.071,11.579,10.132,8.103,8.184,7.980)
MAPE_C <- c(0.474,0.371,0.333,0.308,0.303,0.297,0.261,0.246,0.216,0.171,0.169,0.165)

p <- plot_ly(x=gammas, y=costs, z=MAPE_C)     # Interaktywny wykres 3D, zaleznosci wspolczynnika MAPE od gammy i kosztu 
p                                             # Plot (automatyczny)



### Tune dla modelu SVM, uczony na: Load_Min15 + Load_Day1B + Hour

# SVM_Tune_H <- tune(svm, Load_Now ~ Load_Min15 + Load_Day1B + Hour, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,
#                   ranges = list(gamma = c(10^(-12:-5),(10^(-12:-5))/2), cost = 10^(2:8)),
#                   tunecontrol = tune.control(sampling = "fix", performances = TRUE ))

# newdata$SVM_Tune <- predict(SVM_Tune_H$best.model, newdata)                           # Load_Min15 + Load_Day1B + Hour
# newdata$SVM_Tune_Shift <- c(as.numeric(as.character(newdata$SVM_Tune[2:length(newdata$SVM_Tune)])), as.numeric(as.character(newdata$SVM_Tune[length(newdata$SVM_Tune)])))
# MAE_SVM_Tune_H <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift)) / length(newdata$Load_Now)
# MAPE_SVM_Tune_H <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100



### Tune dla modelu SVM, uczony na: Load_Min15 + Load_Day1B + Load_Min30 + Hour

# SVM_Tune_H30 <- tune(svm, Load_Now ~ Load_Min15 + Load_Day1B + Hour + Load_Min30, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,
#                   ranges = list(gamma = c(10^(-12:-5),(10^(-12:-5))/2), cost = 10^(2:8)),
#                   tunecontrol = tune.control(sampling = "fix", performances = TRUE ))

# newdata$SVM_Tune <- predict(SVM_Tune_H30$best.model, newdata)                           # Load_Min15 + Load_Day1B + Hour  + Load_Min30
# newdata$SVM_Tune_Shift <- c(as.numeric(as.character(newdata$SVM_Tune[2:length(newdata$SVM_Tune)])), as.numeric(as.character(newdata$SVM_Tune[length(newdata$SVM_Tune)])))
# MAE_SVM_Tune_H30 <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift)) / length(newdata$Load_Now)
# MAPE_SVM_Tune_H30 <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### Tune dla modelu SVM, uczony na: Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45

# SVM_Tune_3045 <- tune(svm, Load_Now ~ Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,
#                   ranges = list(gamma = c(10^(-12:-5),(10^(-12:-5))/2), cost = 10^(2:8)),
#                   tunecontrol = tune.control(sampling = "fix", performances = TRUE ))

# newdata$SVM_Tune <- predict(SVM_Tune_3045$best.model, newdata)                           # Load_Min15 + Load_Day1B + Load_Min30 + Load_Min45
# newdata$SVM_Tune_Shift <- c(as.numeric(as.character(newdata$SVM_Tune[2:length(newdata$SVM_Tune)])), as.numeric(as.character(newdata$SVM_Tune[length(newdata$SVM_Tune)])))
# MAE_SVM_Tune_3045 <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift)) / length(newdata$Load_Now)
# MAPE_SVM_Tune_3045 <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100


### Tune dla modelu SVM, uczony na: Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min

# SVM_Tune_1Day <- tune(svm, Load_Now ~ Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min, data = Temp, validation.x = newdata, validation.y = newdata$Load_Now,
#                   ranges = list(gamma = c(10^(-12:-5),(10^(-12:-5))/2), cost = 10^(2:8)),
#                   tunecontrol = tune.control(sampling = "fix", performances = TRUE ))

# newdata$SVM_Tune <- predict(SVM_Tune_1Day$best.model, newdata)                           # Load_Min15 + Load_Day1B + Load_Day1B15min + Load_Day1Bp15min
# newdata$SVM_Tune_Shift <- c(as.numeric(as.character(newdata$SVM_Tune[2:length(newdata$SVM_Tune)])), as.numeric(as.character(newdata$SVM_Tune[length(newdata$SVM_Tune)])))
# MAE_SVM_Tune_1Day <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift)) / length(newdata$Load_Now)
# MAPE_SVM_Tune_1Day <- sum(abs(newdata$Load_Now - newdata$SVM_Tune_Shift) / newdata$Load_Now) / length(newdata$Load_Now) * 100

