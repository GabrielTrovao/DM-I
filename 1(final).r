#setwd("/Users/gabriellima/Documents/4ºano/1ºSEMESTRE/DATA MINING I/PROJETO/SITE1")
#setwd("/home/deyl/Desktop/fcup/DMI/work/DM-I-master")
#setwd(dir = getwd())
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

library(ggbiplot)
library(dplyr)
library(dlookr)
library(ggplot2)
library(e1071)
library(DMwR)
library(zoo)
library(rstudioapi)
library(here)
library(devtools)
library(lubridate)
library(forecast)
library(data.table)
library(mltools)
library(ranger)
library(neuralnet)

#THIS DATASET CONCERNS THE Aotizhongxin STATION
site1 <- read.csv("PRSA_Data_Aotizhongxin_20130301-20170228.csv",header = TRUE)

#DATA PRE-PROCESSING AND CLEANING
anyNA(site1$PM10) #TRUE

filter(site1,day>31)

#REMOVING POINTLESS COLUMNS 
site1 <- select(site1,-"station")

site1tbl <- tbl_df(site1)
site1tbl
site1$date  <- as.Date(with(site1, paste(year, month, day,sep="-")), "%Y-%m-%d")
site1$date
site1 <- select(site1,-"No",-"year",-"month",-"day")
site1 <- site1 %>% select(date,everything())


##################################

#APPROX PARA AS COLUNAS
site1$PM10 <- na.approx(site1$PM10)
site1$PM2.5 <- na.approx(site1$PM2.5)
site1$SO2 <- na.approx(site1$SO2)
site1$NO2 <- na.approx(site1$NO2)
site1$CO <- na.approx(site1$CO)
site1$O3 <- na.approx(site1$O3)
site1$TEMP <- na.approx(site1$TEMP)
site1$PRES <- na.approx(site1$PRES)
site1$DEWP <- na.approx(site1$DEWP)
site1$wd <- na.locf(site1$wd)
site1$WSPM <- na.approx(site1$WSPM)
site1$RAIN <- na.approx(site1$RAIN)

site1 %>% filter_all(any_vars(is.na(.))) 

#PCA 
site1_pca <- prcomp(site1[,c(2:12,14)], center = TRUE, scale. = TRUE)
summary(site1_pca)
ggbiplot(site1_pca)

site1_pca

#Feature Engineering

AQICalc <- function(v, vals) {
  IAQI <- c(50,100,150,200,300,400,500)
  i <- 1
  for (variable in vals) {
    if (v <= variable) {
      if (i==1) {
        Ilo <- 0
        Ihi <- IAQI[i]
        BPhi <- vals[i]
        BPlo <- 0
      } else {
        Ihi <- IAQI[i]
        Ilo <- IAQI[i-1]
        BPhi <- vals[i]
        BPlo <- vals[i-1]
      }
      break
    }
    i <- i + 1
  }
  if(i > 7) {
    Ip <- 500
  }
  else {
    Ip <- ((Ihi - Ilo) / (BPhi - BPlo)) * (v - BPlo) + Ilo
  }
  Ip
}

AQIO3 <- function(v) {
  ozone_vals <- c(160,200,300,400,800,1000,1200)
  AQICalc(v, ozone_vals)
}

AQISO2 <- function(v) {
  so2_vals <- c(50,150,475,800,1600,2100,2620)
  AQICalc(v, so2_vals)
}

AQINO2 <- function(v) {
  no2_vals <- c(40,80,180,280,565,750,940)
  AQICalc(v, no2_vals)
}

AQICO <- function(v) {
  co_vals <- c(2000,4000,14000,24000,36000,48000,60000)
  AQICalc(v, co_vals)
}

AQIPM10 <- function(v) {
  pm10_vals <- c(50,150,250,350,420,500,600)
  AQICalc(v, pm10_vals)
}

getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

AQIcomps <- setNames(aggregate(site1$PM2.5,by=list(date=site1$date), FUN = mean), c("date","PM2.5"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$PM10,by=list(date=site1$date), FUN = mean),c("date","PM10")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$SO2,by=list(date=site1$date), FUN = mean),c("date","SO2")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$NO2,by=list(date=site1$date), FUN = mean),c("date","NO2")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$CO,by=list(date=site1$date), FUN = mean),c("date","CO")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$O3,by=list(date=site1$date), FUN = mean),c("date","O3")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$TEMP,by=list(date=site1$date), FUN = mean),c("date","TEMP")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$PRES,by=list(date=site1$date), FUN = mean),c("date","PRES")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$DEWP,by=list(date=site1$date), FUN = mean),c("date","DEWP")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$RAIN,by=list(date=site1$date), FUN = mean),c("date","RAIN")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$wd,by=list(date=site1$date), FUN = getmode),c("date","wd")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$WSPM,by=list(date=site1$date), FUN = mean),c("date","WSPM")), by = c("date"))
AQIcomps

#Map Median Values to get IAQI

AQIcomps$PM10 <- sapply(AQIcomps$PM10, FUN = AQIPM10)
AQIcomps$SO2 <- sapply(AQIcomps$SO2, FUN = AQISO2)
AQIcomps$NO2 <- sapply(AQIcomps$NO2, FUN = AQINO2)
AQIcomps$CO <- sapply(AQIcomps$CO, FUN = AQICO)
AQIcomps$O3 <- sapply(AQIcomps$O3, FUN = AQIO3)
AQIcomps$AQI <- apply(AQIcomps[3:7],1,max)
AQIcomps

#Try to express correlation between multiple values and AQI


divideByMonth <- function(m) {
  x <- month(ymd(m))
  if(x == 1)
    month = "January"
  else if (x == 2)
    month = "February"
  else if (x == 3)
    month = "March"
  else if (x == 4)
    month = "April"
  else if (x == 5)
    month = "May"
  else if (x == 6)
    month = "June"
  else if (x == 7)
    month = "July"
  else if (x == 8)
    month = "August"
  else if (x == 9)
    month = "September"
  else if (x == 10)
    month = "October"
  else if (x == 11)
    month = "November"
  else if (x == 12)
    month = "December"
}

AQIbydate <- AQIcomps %>% select("date","AQI")
AQIbydate$day <- as.numeric(format(as.Date(AQIbydate$date),"%d"))
AQIbydate$month <- sapply(AQIcomps$date, FUN = divideByMonth)
AQIbydate$month_f <- factor(AQIbydate$month, levels=c("January","February","March","April","May","June","July","August","September","October","November", "December"))


ggplot(AQIcomps,aes(x = PM2.5, y = AQI))+ geom_point() + 
                    ylim(1,500) + theme_linedraw() + ggtitle("PM2.5/AQI Correlation") #definitely correlated
ggplot(AQIcomps,aes(x = TEMP, y = AQI))+ geom_point() + 
                    ylim(1,500) + theme_linedraw() + ggtitle("Temperature/AQI Correlation") #not much relevant
ggplot(AQIcomps,aes(x = DEWP, y = AQI))+ geom_point() + 
                    ylim(1,500) + theme_linedraw() + theme_classic() + ggtitle("DEWP/AQI Correlation") #not much relevant
ggplot(AQIcomps,aes(x = TEMP, y = DEWP))+ geom_point() + 
                    theme_linedraw() + ggtitle("Temperature/DEWP Correlation") # definitely a connection
ggplot(AQIcomps,aes(x = AQI, y = RAIN))+ geom_point() + xlim(1,500) +
                    theme_linedraw() + ggtitle("RAIN/AQI Correlation") #rain may help AQI go down
ggplot(AQIcomps,aes(x = PRES, y = AQI))+ geom_point() +
                    ylim(1,500) + theme_linedraw() + ggtitle("PRES/AQI Correlation") #little apparent relation
ggplot(AQIcomps,aes(x = PRES, y = TEMP))+ geom_point() + 
                    theme_linedraw() + ggtitle("PRES/TEMP Correlation") #inverse proportionality exists
ggplot(AQIcomps,aes(x = WSPM, y = AQI)) + geom_point() + ylim(1,500) +
                    theme_linedraw() + facet_wrap(~wd) + ggtitle("Wind/AQI Correlation") # NE direction with low speed has some instances with high AQI


ggplot(AQIbydate, aes(day, 
                      AQI, group=factor(year(date)), colour=factor(year(date)))) +
  facet_wrap(~month_f, scales = "free") +
  geom_line() +
  labs(x="Month", colour="Year") +
  ylim(1,500) +
  scale_x_discrete(breaks = 1:10) +
  theme_classic()

#Prediction Models

#start by one-hot encoding wind direction

PredictionDB <- select(AQIcomps, -"PM10", -"SO2", -"NO2", -"CO", -"O3")
PredictionDB <- as.data.frame(one_hot(as.data.table(PredictionDB)))

#next proceed to data normalization

normalize <- function(x){
  normalized_x <- (x - min(x))/(max(x)-min(x))
  normalized_x <- round(normalized_x*100)/100
  normalized_x
}

PredictionDB$PM2.5 <- normalize(PredictionDB$PM2.5)
PredictionDB$TEMP <- normalize(PredictionDB$TEMP)
PredictionDB$PRES <- normalize(PredictionDB$PRES)
PredictionDB$DEWP <- normalize(PredictionDB$DEWP)
PredictionDB$RAIN <- normalize(PredictionDB$RAIN)
PredictionDB$WSPM <- normalize(PredictionDB$WSPM)
PredictionDB
#make neural network


#make randomn forest

RF <- ranger(AQI ~ PM2.5 + TEMP + PRES + DEWP + RAIN
               + wd_E + wd_ENE + wd_ESE + wd_N + wd_NE + wd_NNE +
               + wd_NNW + wd_NW + wd_S + wd_SE + wd_SSE + wd_SSW +
               + wd_SW + wd_W + wd_WNW + wd_WSW + WSPM
             ,data = PredictionDB ,num.trees = 100, importance = "none", write.forest = TRUE,min.node.size = 10)
RF

getAQILabel <- function(x) {
  AQIvalues <- c(50,100,150,200,300,400,500)
  AQIlabels <- c("Good","Moderate","Unhealthy for Sensitive Groups","Unhealthy","Very Unhealthy","Hazardous","Very Hazardous")
  i <- 1
  while(x > AQIvalues[i]){
    i <- i + 1
  }
  AQIlabels[i]
}

Correlation <- select(PredictionDB, "date", "AQI")
Correlation$AQIlabels <- sapply(Correlation$AQI, FUN = getAQILabel)
Correlation$Predictions <- RF$predictions
Correlation$PredLabel <- sapply(Correlation$Predictions, FUN = getAQILabel)
Correlation
LabelAccuracy <- nrow(subset(Correlation,AQIlabels == PredLabel,PredLabel))/nrow(Correlation)
LabelAccuracy


save(site1,file = "firstSite.RData")
