library(dplyr)

Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")

createDataFrame <- function () {
  hpc <- read.csv("household_power_consumption.txt", sep = ";", na.strings = "?", stringsAsFactors = FALSE)
  from <- strptime("01/02/2007 00:00:00", format = "%d/%m/%Y %H:%M:%S")
  to <- strptime("02/02/2007 23:59:59", format = "%d/%m/%Y %H:%M:%S")
  hpc <- transform(hpc, datetime = strptime(paste(Date,Time), format = "%d/%m/%Y %H:%M:%S"))
  subset(hpc, datetime >= from & datetime <= to)
}

createPlot <- function(dataframe) {
  png("plot4.png",height = 480, width = 480)
  par(mfrow = c(2,2))
  
  # plot 1
  plot(hpc$datetime, hpc$Global_active_power, ylab = "Global Active Power", xlab = "", type = "n")
  lines(hpc$datetime, hpc$Global_active_power)
  
  # plot 2
  plot(hpc$datetime, hpc$Voltage, ylab = "Voltage", xlab = "datetime", type = "n")
  lines(hpc$datetime, hpc$Voltage)
  
  # plot 3
  plot(hpc$datetime, hpc$Sub_metering_1, ylab = "Energy sub metering", xlab = "", type = "n")
  lines(hpc$datetime, hpc$Sub_metering_1, col = "black")
  lines(hpc$datetime, hpc$Sub_metering_2, col = "red")
  lines(hpc$datetime, hpc$Sub_metering_3, col = "blue")
  legend("topright", legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), col = c("black","red","blue"), lty = 1, bty = "n")
  
  # plot 4
  plot(hpc$datetime, hpc$Global_reactive_power, ylab = "Global_reactive_power", xlab = "datetime", type = "n")
  lines(hpc$datetime, hpc$Global_reactive_power)
  
  dev.off()
  
  par(mfrow = c(1,1))
}

createDataFrame() %>%
  createPlot