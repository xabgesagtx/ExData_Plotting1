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
  png("plot3.png",height = 480, width = 480)
  plot(hpc$datetime, hpc$Sub_metering_1, ylab = "Energy sub metering", xlab = "", type = "n")
  lines(hpc$datetime, hpc$Sub_metering_1, col = "black")
  lines(hpc$datetime, hpc$Sub_metering_2, col = "red")
  lines(hpc$datetime, hpc$Sub_metering_3, col = "blue")
  legend("topright", legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), col = c("black","red","blue"), lty = 1)
  dev.off()
}

createDataFrame() %>%
  createPlot