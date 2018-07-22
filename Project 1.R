household_power_consumption <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

## Format date to Type Date
household_power_consumption$Date <- as.Date(household_power_consumption$Date, "%d/%m/%Y")

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
household_power_consumption <- subset(household_power_consumption,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Remove incomplete observation
household_power_consumption <- household_power_consumption[complete.cases(household_power_consumption),]

## Combine Date and Time column
dateTime <- paste(household_power_consumption$Date, household_power_consumption$Time)

## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column
household_power_consumption <- household_power_consumption[ ,!(names(household_power_consumption) %in% c("Date","Time"))]

## Add DateTime column
household_power_consumption <- cbind(dateTime, household_power_consumption)

## Format dateTime Column
household_power_consumption$dateTime <- as.POSIXct(dateTime)

## PLOT 1
## Create the histogram
hist(household_power_consumption$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

## To save file and close the device for plot 1
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()


## PLOT 2
plot(household_power_consumption$Global_active_power~household_power_consumption$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

## To save file and close the device for plot 2
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()


## PLOT 3
with(household_power_consumption, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

## To save file and close the device for plot 3
dev.copy(png,"plot3.png", width=480, height=480)
dev.off()

## PLOT 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(household_power_consumption, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})

## To save file and close the device for plot 4
dev.copy(png,"plot4.png", width=480, height=480)
dev.off()
