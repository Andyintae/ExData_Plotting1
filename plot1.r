library(tidyverse)
library(lubridate)


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


# 3. Plot1 ------------------------------------------------------------

plot1 <- power_n %>% ggplot(aes(Global_active_power))+
  geom_histogram(bins = 18, fill = "#FF0033", col = "black") +
  ggtitle("Global Active Power")+
  labs(x = "Global active power (kilowatts)", y = "Frequency") +
  scale_x_continuous(breaks = seq(0,6,2))+
  scale_y_continuous(breaks = seq(0,1200,200))+
  theme_classic()



# 4. save to PNG file -----------------------------------------------------

dev.cur()

png(file = "plot1.PNG",width=480, height=480)
plot1

dev.off()
