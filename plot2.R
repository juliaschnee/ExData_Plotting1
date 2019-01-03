create_plot2 <- function(){
  #Function input is Household Power Consumption data, subset data for the dates 2007-02-01 and 2007-02-02.
  #Function output is a PNG histogram of global active power.
  
  #Prepare a class to read Date column as Date
  setClass('myDate')
  setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y"))
  
  #Read in the data
  hpc <- read.table("household_power_consumption.txt", 
                    sep = ";", 
                    na.strings = "?", 
                    header = TRUE, 
                    stringsAsFactors = FALSE, 
                    colClasses = c("myDate", "character", 
                                   "numeric", "numeric", 
                                   "numeric", "numeric", 
                                   "numeric", "numeric", "numeric"))
  
  #Subset two dates as per the task
  sub_hpc <- subset(hpc, hpc$Date == "2007-02-01" | hpc$Date == "2007-02-02")
  sub_hpc$DateTime <- strptime(paste(sub_hpc$Date, sub_hpc$Time), format = "%Y-%m-%d %H:%M:%S")
  
  #Create PNG graphic device
  png(filename = "plot2.png",
      width = 480, 
      height = 480, 
      units = "px")
  
  #Create line plot
  plot(sub_hpc$DateTime, sub_hpc$Global_active_power, 
       type = "l", 
       xlab = "", 
       ylab = "Global active power (kilowatts)")
  
  #Close PNG device
  dev.off()
}
