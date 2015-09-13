# Plots 4 charts in a 2 by 2 layout
plot4 <- function () {
        # read data header
        d5rows <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", nrows = 5, na.strings = "?")
        
        # get column names and store in variable 'n'
        n <- colnames(d5rows)
        
        # load the data.table package
        library(data.table)
        
        # use fread function and skip rows until 1st of feb 2007 is found, then begin reading and read
        # the next 2880 rows (i.e. number of minutes in 2 days) storing data in variable 'd'
        d <- as.data.frame(fread("household_power_consumption.txt", sep = ";", skip = "1/2/2007", nrows = 2880, na.strings = "?"))
        
        # rename column names correctly
        setnames(d, colnames(d5rows))
        
        #Combine the Date and Time character variables and then convert them into a new varable of type POSIXlt i.e. Date/Time
        d$Date_Time <- strptime(paste(d$Date, d$Time), "%d/%m/%Y %H:%M:%S")
        
        # initialise png device
        png(filename = "plot4.png")
        
        par(mfrow = c(2, 2), bg = "transparent")
        
        # Call the plot fuction in appropriate sequence
        plot2(d)
        plot5(d)
        plot3(d)
        plot6(d)

        # get range of the Date_Time variable
        r <- range(d$Date_Time)
        
        # Use the axis.POSIXct function to plot the x-axis using the range by days and label with abbreviated days
        axis.POSIXct(1, seq(r[1], r[2], by = "days"), format = "%a")
        
        # turn off device
        dev.off()
}

# Plots Global Active Power for the two days observed
plot2 <- function (d) {
        # plot Global_Active_power whilst suppressing the x-axis
        with (d, plot(Date_Time, Global_active_power, type = "l", xaxt = "n", ylab = "Global Active Power", xlab = ""))
        
        # get range of the Date_Time variable
        r <- range(d$Date_Time)
        
        # Use the axis.POSIXct function to plot the x-axis using the range by days and label with abbreviated days
        axis.POSIXct(1, seq(r[1], r[2], by = "days"), format = "%a")
}

# Plots Energy Sub Metering for the two days observed
plot3 <- function (d) {
        # plot Sub_metering_1 whilst suppressing the x-axis
        plot(d$Date_Time, d$Sub_metering_1, type = "l", xaxt = "n", ylab = "Energy sub metering", xlab = "")
        
        # plot Sub_metering_2 whilst suppressing the x-axis as a red line
        lines(d$Date_Time, d$Sub_metering_2, type = "l", xaxt = "n", col = "red")
        
        # plot Sub_metering_3 whilst suppressing the x-axis as a blue line
        lines(d$Date_Time, d$Sub_metering_3, type = "l", xaxt = "n", col = "blue")
        
        # add legend
        legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1, 1, 1), bty = "n", col = c("black", "red", "blue"))
        
        # get range of the Date_Time variable
        r <- range(d$Date_Time)
        
        # Use the axis.POSIXct function to plot the x-axis using the range by days and label with abbreviated days
        axis.POSIXct(1, seq(r[1], r[2], by = "days"), format = "%a")
}

# Plots Voltage for the two days observed
plot5 <- function (d) {
        # plot Global_Active_power whilst suppressing the x-axis
        with (d, plot(Date_Time, Voltage, type = "l", xaxt = "n", ylab = "Voltage", xlab = "datetime"))
        
        # get range of the Date_Time variable
        r <- range(d$Date_Time)
        
        # Use the axis.POSIXct function to plot the x-axis using the range by days and label with abbreviated days
        axis.POSIXct(1, seq(r[1], r[2], by = "days"), format = "%a")
}

# Plots Global Reactive Power for the two days observed
plot6 <- function (d) {
        # plot Global_Active_power whilst suppressing the x-axis
        with (d, plot(Date_Time, Global_reactive_power, type = "l", xaxt = "n", ylab = "Global_reactive_power", xlab = "datetime"))
        
        # get range of the Date_Time variable
        r <- range(d$Date_Time)
        
        # Use the axis.POSIXct function to plot the x-axis using the range by days and label with abbreviated days
        axis.POSIXct(1, seq(r[1], r[2], by = "days"), format = "%a")
}