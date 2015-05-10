###############################################################################
## Exploratory data analysis, course project 1
##
## Please use a full path or set the working directory when reading the file
###############################################################################
library(sqldf)
library(ggplot2)

plot2 <- function(hpc.df) {
    ## it would be nicer to use qplot
    # qplot(hpc.df$datetime, hpc.df$Global_active_power, ylab = "Global active power (kilowatts)", xlab = "date and time", geom = "line") + theme_bw()
    
    plot(hpc.df$datetime, hpc.df$Global_active_power, type = "n", ylab = "Global active power (kilowatts)", xlab = "") + lines(hpc.df$datetime, hpc.df$Global_active_power)
}

generateplot2 <- function(filenamewithpath) {
    ## Set file and its attributes
    hpcfile <- file(filenamewithpath)
    attr(hpcfile, "file.format") <- list(sep = ";", header = T)
    
    ## read data only for the relevant dates
    hpc.df <- sqldf("select * from hpcfile where Date = '1/2/2007' or Date = '2/2/2007'")
    close(hpcfile)
    
    ## convert date and time strings into a datetime
    hpc.df[,10] <- as.POSIXct(strptime(paste(hpc.df[,1], hpc.df[,2], sep = " "), "%d/%m/%Y %H:%M:%S"))
    names(hpc.df) <- c(names(hpc.df)[1:9], "datetime")
    
    
    png(filename = "plot2.png", width = 480, height = 480, units = "px", bg = "white")
    par(mfrow = c(1,1))
    plot <- plot2(hpc.df)
    print(plot)
    dev.off()
    
    plot
    
}