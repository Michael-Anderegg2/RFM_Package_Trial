# Our first line of code should always "clear Rs brain":
rm(list=ls()) # remove everything from the environment

#load libraries
library(data.table)
library(lubridate)
library(Hmisc)
library(ggplot2)

# load data.table
dt = fread(file = "C:/Users/micha/Desktop/R_big.data.techniques.interactive.visualization/data/transactions.csv", header=TRUE, sep = ",")

#Check the data format
str(dt)

# set to timedate
dt[, TransDate:=dmy(TransDate)]
dt
