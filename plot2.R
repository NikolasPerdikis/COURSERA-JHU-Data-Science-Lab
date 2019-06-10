# Search file for lines with 2007-02-01 and 2007-02-02 dates. 
# Format is dd/mm/yyyy so the string to search is 1/2/2007

input_file <- "D:/household_power_consumption.txt"
startLine <- min(grep("1/2/2007",readLines(input_file)))
stopLine <- min(grep("3/2/2007",readLines(input_file)))

library(lubridate)

# We will read the files 2 times. One will be to get the row names, extract their class, and do update
#Determine the class of the data by reading just a few rows
sample_data <- read.table(input_file,
                          sep=";",
                          header=T,
                          nrows=1)

column_names <- names(sample_data)
classes <- sapply(sample_data, class)

#Create custom class for the first column (the date)
setClass("myDate")
setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
#Create custom class for the first column (the date)
setClass("myTime")
setAs("character","myTime", function(from) strptime(from, format="%T") )

classes[1]="myDate"
classes[2]="myTime"

#Update first and second columns to the desired types (date,time respectively)

HPC_data <- read.table(input_file,
                       skip=startLine-1,
                       nrows=(stopLine-startLine),
                       sep=";",
                       na.strings = "?",
                       col.names=column_names,
                       colClasses = classes)


# Combine Date and Time to a singular field
HPC_data$datetime <- with(HPC_data, 
                           as.POSIXct(paste(HPC_data$Date,hour(HPC_data$Time),minute(HPC_data$Time)),
                                      format="%Y-%m-%d %H %M"))


# Second plot:
plot(Global_active_power ~ datetime,data=HPC_data,type="l",xlab="",ylab="Global Active Power (kilowatts)")

dev.copy(png,file="plot2.png")
dev.off()
