###############################################################################
## Exploratory data analysis, course project 1
##
## Please use a full path or set the working directory when reading the file
###############################################################################
library(sqldf)
library(ggplot2)

plot1 <- function(hpc.df) {
    
    ## It would be nicer to use qplot
    #qplot(hpc.df$Global_active_power, xlab = "Global active power (kilowatts)", ylab = "Frequency") + geom_bar(fill = "red", colour = "black") + theme(panel.background = element_blank())
    hist(hpc.df$Global_active_power, xlab = "Global active power (kilowatts)", col = "red", main = "Global Active Power")
    #title("Global Active Power")
    
}

generateplot1 <- function(filenamewithpath) {
    ## Set file and its attributes
    hpcfile <- file(filenamewithpath)
    attr(hpcfile, "file.format") <- list(sep = ";", header = T)
    
    ## read data only for the relevant dates
    hpc.df <- sqldf("select * from hpcfile where Date = '1/2/2007' or Date = '2/2/2007'")
    close(hpcfile)
    
    ## convert date and time strings into a datetime
    hpc.df[,10] <- as.POSIXct(strptime(paste(hpc.df[,1], hpc.df[,2], sep = " "), "%d/%m/%Y %H:%M:%S"))
    names(hpc.df) <- c(names(hpc.df)[1:9], "datetime")
    
    png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")
    par(mfrow = c(1,1))
    plot <- plot1(hpc.df)
    print(plot)
    dev.off()
    
    plot
    
}