library(tidyverse)


# 1. Create wd, Download and unzip file ----------------------------------------------

if(!file.exists("EDA")){dir.create("EDA")}
setwd("./EDA")
wd <- getwd()

file <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(file, destfile = paste0(wd,"/household_power_consumption.zip"))
unzip(zipfile = "household_power_consumption.zip")


