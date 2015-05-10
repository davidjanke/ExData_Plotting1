###############################################################################
## Exploratory data analysis, course project 1
##
## Please use a full path or set the working directory when reading the file
##
## It will reuse the results from plots 1-3
###############################################################################
library(sqldf)
library(ggplot2)

source('plot1.R')
source('plot2.R')
source('plot3.R')

plot4 <- function(hpc.df) {
    par(mfrow = c(2,2))
    plot1(hpc.df)
    plot2(hpc.df)
    plot3(hpc.df)
    plot(hpc.df$datetime, hpc.df$Global_reactive_power, type = "n", ylab = "Global active power (kilowatts)", xlab = "") + lines(hpc.df$datetime, hpc.df$Global_reactive_power)
}    

generateplot4 <- function(filenamewithpath) {
    ## Set file and its attributes
    hpcfile <- file(filenamewithpath)
    attr(hpcfile, "file.format") <- list(sep = ";", header = T)
    
    ## read data only for the relevant dates
    hpc.df <- sqldf("select * from hpcfile where Date = '1/2/2007' or Date = '2/2/2007'")
    close(hpcfile)
    
    ## convert date and time strings into a datetime
    hpc.df[,10] <- as.POSIXct(strptime(paste(hpc.df[,1], hpc.df[,2], sep = " "), "%d/%m/%Y %H:%M:%S"))
    names(hpc.df) <- c(names(hpc.df)[1:9], "datetime")
    
    png(filename = "plot4.png", width = 480, height = 480, units = "px", bg = "white")
    par(mfrow = c(2,2))
    plot <- plot4(hpc.df)
    print(plot)
    dev.off()
    
    plot
    
}