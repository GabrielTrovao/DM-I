setwd("/Users/gabriellima/Documents/4ºano/1ºSEMESTRE/DATA MINING I/PROJETO/SITE1")

library(dplyr)
library(dlookr)
library(ggplot2)
library(e1071)
library(DMwR)

#THIS DATASET CONCERNS THE Aotizhongxin STATION
site1 <- read.csv("PRSA_Data_Aotizhongxin_20130301-20170228.csv",header = TRUE)

save(site1,file = "firstSite.RData")

#DATA PRE-PROCESSING AND CLEANING
anyNA(site1) #TRUE

filter(site1,day>31)

#REMOVING POINTLESS COLUMNS 
site1 <- select(site1,-"station")

site1tbl <- tbl_df(site1)
site1tbl
site1$date  <- paste(site1$year,site1$month,site1$day,sep = "/")
site1$date
site1 <- select(site1,-"year",-"month",-"day")
site1 <- site1[,c(1,15,2:14)]
