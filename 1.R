setwd("/Users/gabriellima/Documents/4ºano/1ºSEMESTRE/DATA MINING I/PROJETO/SITE1")
#setwd("/home/deyl/Desktop/fcup/DMI/work/DM-I-master")
setwd(dir = getwd())

library(dplyr)
library(dlookr)
library(ggplot2)
library(e1071)
library(DMwR)
library(zoo)
library(rstudioapi)
library(here)
library(devtools)
library(ggbiplot)
library(tidyimpute)
library(fpc)
library(factoextra)
library(cluster)


#THIS DATASET CONCERNS THE Aotizhongxin STATION
site1 <- read.csv("PRSA_Data_Aotizhongxin_20130301-20170228.csv",header = TRUE)
#site1ST <- read.csv("PRSA_Data_Aotizhongxin_20130301-20170228.csv",header = TRUE)

#DATA PRE-PROCESSING AND CLEANING
anyNA(site1$PM10) #TRUE
filter(site1,day>31)

#REMOVING POINTLESS COLUMNS 
site1 <- select(site1,-"station")
site1tbl <- tbl_df(site1)
site1tbl
site1$date  <- paste(site1$year,site1$month,site1$day,sep = "/")
site1$date
site1 <- select(site1,-"No",-"year",-"month",-"day")
site1 <- site1 %>% select(date,everything())
site1

#COLUNA PM10
anyNA(na.spline(site1$PM10))
site1$PM10 <- na.spline(site1$PM10)
anyNA(site1$PM10)

#LINEAR INTERPOLATION - NAO FAZER
na.approx(site1$PM2.5)
anyNA(na.approx(site1$PM2.5))
##################################

#SPLINES PARA AS RESTANTES COLUNAS
site1$PM2.5 <- na.spline(site1$PM2.5)
site1$SO2 <- na.spline(site1$SO2)
site1$NO2 <- na.spline(site1$NO2)
site1$CO <- na.spline(site1$CO)
site1$O3 <- na.spline(site1$O3)
site1$TEMP <- na.spline(site1$TEMP)
site1$PRES <- na.spline(site1$PRES)
site1$DEWP <- na.spline(site1$DEWP)
#site1$RAIN <- na.spline(site1$RAIN)
#site1$WSPM <- na.spline(site1$WSPM)
site1$wd <- na.locf(site1$wd)
site1$WSPM <- na.approx(site1$WSPM)
site1$RAIN <- na.approx(site1$RAIN)

site1 %>% filter_all(any_vars(is.na(.))) 


#PCA 
site1_pca <- prcomp(site1[,c(3:11,14)], center = TRUE, scale. = TRUE)
summary(site1_pca)
ggbiplot(site1_pca,choices = 1:2)


#TESTAR PCA PARA DIFERENTES AMOSTRAS DOS DADOS
#AINDA NAO FOI APLICADO STRATIFIED SAMPLING
#APENAS TEMOS AS PARTICOES
site1_pca
site1 %>% mutate(site1ST[,15])
colnames(site1)[15] <- "year"
site1
yearOne <- site1 %>% subset(year==2013)
yearTwo <- site1 %>% subset(year==2014)
yearThree <- site1 %>% subset(year==2015)
yearFour <- site1 %>% subset(year==2016)
yearFive <- site1 %>% subset(year==2017)

siteYear1_pca <- prcomp(yearOne[,c(3:11,14)], center = TRUE, scale. = TRUE)
summary(siteYear1_pca)
ggbiplot(siteYear1_pca,choices = 1:2)

siteYear2_pca <- prcomp(yearTwo[,c(3:11,14)], center = TRUE, scale. = TRUE)
summary(siteYear2_pca)
ggbiplot(siteYear2_pca,choices = 1:2)

siteYear3_pca <- prcomp(yearThree[,c(3:11,14)], center = TRUE, scale. = TRUE)
summary(siteYear3_pca)
ggbiplot(siteYear3_pca,choices = 1:2)

siteYear4_pca <- prcomp(yearFour[,c(3:11,14)], center = TRUE, scale. = TRUE)
summary(siteYear4_pca)
ggbiplot(siteYear4_pca,choices = 1:2)

siteYear5_pca <- prcomp(yearFive[,c(3:11,14)], center = TRUE, scale. = TRUE)
summary(siteYear5_pca)
ggbiplot(siteYear5_pca,choices = 1:2)
############################################################################

#TENTATIVA DE APLICAR O METODO K-MEANS PARA CLUSTERING






save(site1,file = "firstSite.RData")
