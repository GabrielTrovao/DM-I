setwd("/Users/gabriellima/Documents/4ºano/1ºSEMESTRE/DATA MINING I/PROJETO/SITE1")
setwd("/home/deyl/Desktop/fcup/DMI/work/DM-I-master")
setwd(dir = getwd())

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
site1




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

AQIcomps <- setNames(aggregate(site1$PM10,by=list(date=site1$date), FUN = sum), c("date","PM10"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$SO2,by=list(date=site1$date), FUN = sum),c("date","SO2")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$NO2,by=list(date=site1$date), FUN = sum),c("date","NO2")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$CO,by=list(date=site1$date), FUN = mean),c("date","CO")), by = c("date"))
AQIcomps <- merge(AQIcomps, setNames(aggregate(site1$O3,by=list(date=site1$date), FUN = mean),c("date","O3")), by = c("date"))
AQIcomps$AQI <- apply(AQIcomps[2:6],1,max)
AQIcomps




save(site1,file = "firstSite.RData")