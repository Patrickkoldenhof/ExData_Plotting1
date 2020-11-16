fileurl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileurl, destfile = "Household_power_consumption.zip", mode = "wb" )
unzip("Household_power_consumption.zip")

##Read data
Household_data <- read.table("household_power_consumption.txt", sep = ";", na.strings = "?", header = TRUE, 
                             colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
##Transform dates to class Date
library(lubridate)
Household_data$Date <- dmy(Household_data$Date)

##Only subset the 2007-02-01 and 2007-02-02 dates
Household_datasubsetted <- Household_data[Household_data$Date == "2007-02-01" | Household_data$Date == "2007-02-02", ]

##Combine Date and Time and transform it to POSIXct
datetime <- paste(Household_datasubsetted$Date, Household_datasubsetted$Time)
datetime <- ymd_hms(datetime)
Household_datasubsetted <- cbind(datetime, Household_datasubsetted)

##Remove the original Date and Time class
library(dplyr)
Household_datasubsetted <- Household_datasubsetted %>% select("datetime", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity",
                                                              "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

##Plot 4
par(mfrow=c(2,2), mar = c(4,4,2,1), oma = (0,0,2,0))
with(Household_datasubsetted, {
    plot(Global_active_power~datetime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
    plot(Voltage~datetime, type="l", ylab="Voltage (volt)", xlab="")
    plot(Sub_metering_1 ~ datetime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")    
    lines(Sub_metering_2 ~ datetime, col = "Red")
    lines(Sub_metering_3 ~ datetime, col = "Blue")
    legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2,
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    plot(Global_reactive_power ~ datetime, type = "l", ylab = "Global Reactive Power (kilowatts)", xlab="" )
})

dev.copy(png,"plot4.png", width=480, height=480)
dev.off()

