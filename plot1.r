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


# 3. Plot1 ------------------------------------------------------------

plot1 <- power_n %>% ggplot(aes(Global_active_power))+
  geom_histogram(bins = 18, fill = "#FF0033") +
  labs(x = "Global active power (kilowatts)", y = "Frequency", title = "Global Active Power") +
  scale_x_continuous(breaks = seq(0,6,2))+
  scale_y_continuous(breaks = seq(0,1200,200))+
  theme_classic()


# 4. Plot2 ----------------------------------------------------------------

plot2 <- power_n %>% ggplot(aes(Date_time, Global_active_power))+
  geom_line()+
  scale_x_datetime(date_labels = "%a %H") +
  labs(x = "", y = "Global active power (kilowatts)")


# 5. Plot3  ---------------------------------------------------------------

plot3 <- power_long %>% ggplot(aes(Date_time, reading, col = Sub_metering))+
  geom_line()+
  scale_x_datetime(date_labels = "%a %H") +
  labs(x = "", y = "Energy sub metering")+
  theme(legend.position = c(0.9,0.9))



# 6. Plot4 ----------------------------------------------------------------

plot4 <- power_n %>% ggplot(aes(Date_time, Voltage))+
  geom_line()+
  scale_x_datetime(date_labels = "%a %H") +
  labs(x = "datetime", y = "Voltage")

plot5 <- power_n %>% ggplot(aes(Date_time, Global_reactive_power))+
  geom_line()+
  scale_x_datetime(date_labels = "%a %H") +
  labs(x = "datetime", y = "Global active power")

grid.arrange(plot2, plot4, plot3, plot5, nrow = 2, ncol = 2)


plot...

dev.cur()
png(file = "")
## plot redrow...

dev.copy(png, "geyserplot.png")

dev.off()

?abline
