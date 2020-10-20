#import data to Global environment as txt----
df <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))


#  Date----
df$Date <- as.Date(df$Date, "%d/%m/%Y")



# Remove obseravations----
df<- df[complete.cases(df),]

# Filter data set from Feb. 1, 2007 to Feb. 2, 2007
df <- subset(df,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))


#histogram----

hist(df$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

#save plot1----

dev.copy(png,"plot1_ad.png", width=480, height=480)

# Combine Date and Time column----
dateTime <- paste(df$Date, df$Time)

# Name the vector
dateTime <- setNames(dateTime, "DateTime")

# Remove Date and Time column----
df <- df[ ,!(names(df) %in% c("Date","Time"))]

# DateTime column----
df <- cbind(dateTime, df)

## Format dateTime Column
df$dateTime <- as.POSIXct(dateTime)
# Plot 2----
plot(df$Global_active_power~df$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

#Plot 3----
with(df, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
#plot4----
#a----
par(mfrow = c(2, 2)) 
#b-----
plot(df$dateTime, df$Global_active_power, type="l", xlab="", ylab="Global Active Power", cex=0.2)
plot(df$dateTime, df$Voltage, type="l", xlab="datetime", ylab="Voltage")
# c----
plot(df$dateTime, df$Sub_metering_1, type="l", ylab="Energy Submetering", xlab="")
lines(df$dateTime, df$Sub_metering_2, type="l", col="red")
lines(df$dateTime, df$Sub_metering_3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=, lwd=2.5, col=c("black", "red", "blue"), bty="o")
# d----
plot(df$dateTime, df$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power", cex=0.2)
