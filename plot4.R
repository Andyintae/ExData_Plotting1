library(tidyverse)
library(lubridate)
library(gridExtra)


# 1. Create wd, Download and unzip file ----------------------------------------------

if(!file.exists("EDA")){dir.create("EDA")}
setwd("./EDA")
wd <- getwd()

file <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(file, destfile = paste0(wd,"/household_power_consumption.zip"))
unzip(zipfile = "household_power_consumption.zip")

power <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")


# 2. Cleaning data --------------------------------------------------------

power_n <- power %>% 
  mutate(Date = dmy(Date),
         Date_time = ymd_hms(paste(Date, Time))) %>% 
  mutate_at(c("Global_active_power", "Global_reactive_power", "Voltage",
              "Global_intensity", "Sub_metering_1", 'Sub_metering_2', "Sub_metering_3")
            , as.numeric)%>% 
  filter(Date == "2007-02-01" | Date == "2007-02-02")


power_long <- power %>% 
  mutate(Date = dmy(Date),
         Date_time = ymd_hms(paste(Date, Time))) %>% 
  mutate_at(c("Global_active_power", "Global_reactive_power", "Voltage",
              "Global_intensity", "Sub_metering_1", 'Sub_metering_2', "Sub_metering_3")
            , as.numeric)%>% 
  gather(key = Sub_metering, value = reading, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  filter(Date == "2007-02-01" | Date == "2007-02-02")


# 3. Plot 2,3,4,5 ----------------------------------------------------------------

plot4 <- power_n %>% ggplot(aes(Date_time, Global_active_power))+
  geom_line()+
  scale_x_datetime(date_labels = "%a %H") +
  labs(x = "", y = "Global active power")

plot5 <- power_n %>% ggplot(aes(Date_time, Voltage))+
  geom_line()+
  scale_x_datetime(date_labels = "%a %H") +
  labs(x = "datetime", y = "Voltage")

plot6 <- power_long %>% ggplot(aes(Date_time, reading, col = Sub_metering))+
  geom_line()+
  scale_x_datetime(date_labels = "%a %H") +
  labs(x = "", y = "Energy sub metering")+
  theme(legend.position = c(0.9,0.9))

plot7 <- power_n %>% ggplot(aes(Date_time, Global_reactive_power))+
  geom_line()+
  scale_x_datetime(date_labels = "%a %H") +
  labs(x = "datetime", y = "Global active power")


# 4. save to PNG file -----------------------------------------------------

dev.cur()

png(file = "plot4.PNG")
grid.arrange(plot4, plot5, plot6, plot7, nrow = 2, ncol = 2)

dev.off()

