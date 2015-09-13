plot1 <- function () {
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
        
        # initialise png device
        png(filename = "plot1.png")
        
        # set background as transparent
        par(bg = "transparent")
        
        # plot histogram of Global active power
        with (d, hist(Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)", 
             ylab = "Frequency"))
        
        # turn off device
        dev.off()
}