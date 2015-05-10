###############################################################################
## Exploratory data analysis, course project 1
##
## Please use a full path or set the working directory when reading the file
###############################################################################
library(sqldf)
library(ggplot2)

plot3 <- function(hpc.df) {
    plot(hpc.df$datetime, hpc.df$Sub_metering_1, type="n", xlab = "", ylab = "Energy sub metering", ylim = range(hpc.df$Sub_metering_1, hpc.df$Sub_metering_2, hpc.df$Sub_metering_3)) +
              lines(hpc.df$datetime, hpc.df$Sub_metering_1) + 
              lines(hpc.df$datetime, hpc.df$Sub_metering_2, col = "red") + 
              lines(hpc.df$datetime, hpc.df$Sub_metering_3, col = "blue")
              legend("topright", c("Sub metering 1", "Sub metering 2", "Sub metering 3"), lty=c(1,1), lwd=c(2.5, 2.5), col=c("black", "red", "blue"))
}    

generateplot3 <- function(filenamewithpath) {
    ## Set file and its attributes
    hpcfile <- file(filenamewithpath)
    attr(hpcfile, "file.format") <- list(sep = ";", header = T)
    
    ## read data only for the relevant dates
    hpc.df <- sqldf("select * from hpcfile where Date = '1/2/2007' or Date = '2/2/2007'")
    close(hpcfile)
    
    ## convert date and time strings into a datetime
    hpc.df[,10] <- as.POSIXct(strptime(paste(hpc.df[,1], hpc.df[,2], sep = " "), "%d/%m/%Y %H:%M:%S"))
    names(hpc.df) <- c(names(hpc.df)[1:9], "datetime")
    
    
    png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "white")
    par(mfrow = c(1,1))
    plot <- plot3(hpc.df)
    print(plot)
    dev.off()
    
    plot
    
}