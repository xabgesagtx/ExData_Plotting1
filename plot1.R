library(dplyr)

createDataFrame <- function () {
  hpc <- read.csv("household_power_consumption.txt", sep = ";", na.strings = "?", stringsAsFactors = FALSE)
  from <- strptime("01/02/2007 00:00:00", format = "%d/%m/%Y %H:%M:%S")
  to <- strptime("02/02/2007 23:59:59", format = "%d/%m/%Y %H:%M:%S")
  hpc <- transform(hpc, datetime = strptime(paste(Date,Time), format = "%d/%m/%Y %H:%M:%S"))
  subset(hpc, datetime >= from & datetime <= to)
}

createPlot <- function(dataframe) {
  png("plot1.png",height = 480, width = 480)
  hist(hpc$Global_active_power, xlab = "Global Active Power (kilowatts)", main = "Global Active Power", col = "red")
  dev.off()
}

createDataFrame() %>%
  createPlot