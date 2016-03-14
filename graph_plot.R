draw_plots<-function (data_file) {

require(dplyr)
require(gridExtra)
require(ggplot2)

## Read the datafile - Complete file is read - Memory is checked.        
rd1<-read.table("../data/dataset-1/household_power_consumption.txt",nrows=4)
classes<-sapply(rd1,class)
rd2<-read.table("../data/dataset-1/household_power_consumption.txt",
                header = TRUE,sep =";",na.strings = "?",colClasses = classes)
## Convert Date to Date class
rd3<-mutate(rd2,Date =format(as.Date(as.character(rd2$Date),"%d/%m/%Y",
                             "%Y/%m/%d")))
## Hardcoded values - Can pass as variables                
data_str1<-format(as.Date("2007/02/01","%Y/%m/%d"),"%Y/%m/%d")
data_str2<-format(as.Date("2007/02/02","%Y/%m/%d"),"%Y/%m/%d")
data1_rd3<-filter(rd3,rd3$Date == data_str1)
data2_rd3<-filter(rd3,rd3$Date == data_str2)
## Create Relevent Dataset
data3_rd3<-bind_rows(data1_rd3,data2_rd3)
png(file="plot1.png",width = 480,height = 480)
## Create a Histogram - Problem1
hist(data3_rd3$Global_active_power,col="red",main =
         "Global Active Power",xlab = "Global Active Power (Kilowatts)")

## Test
#data4_rd4<-mutate(data3_rd3,datetime=
#                      format(as.Date(data3_rd3$Date,"%Y/%m/%d"),"%a"))

## Add "datetime" column (append date and time) 
## to the dataset and call it data5_rd5

data5_rd5<-mutate(data3_rd3,datetime= 
                      paste(format(as.Date(data3_rd3$Date,"%Y/%m/%d"),
                    "%Y/%m/%d"),data3_rd3$Time))
## Correct the timezone
data5_rd5$datetime<-strptime(data5_rd5$datetime,format = "%Y/%m/%d %H:%M:%S",
                             tz = "America/Los_Angeles")

## Problem 2
## Use ggplot and scale datetime using "days of the week"

g1<-ggplot(data5_rd5,aes(data5_rd5$datetime,data5_rd5$Global_active_power)) + 
    geom_line() + scale_x_datetime(date_labels = "%a",date_breaks = "1 day") +
    ylab("Global Active Power") + xlab("")


## Problem 3

## Create graphs for different sub-metering in different colors

g2<-ggplot(data5_rd5,aes(x=data5_rd5$datetime,y=value,color = variable)) + 
    geom_line(y= data5_rd5$Sub_metering_1,col="Black") +
    geom_line(y=data5_rd5$Sub_metering_2,col="Red")+
    geom_line(y=data5_rd5$Sub_metering_3,col="Blue") + 
    scale_x_datetime(date_labels = "%a",date_breaks = "1 day") +
    ylab("Energy submetering") + xlab("") + ylim(0,38)

## Problem 4

## Plot all graphs in the same grid. 
require(gridExtra)

g3<-ggplot(data5_rd5,aes(data5_rd5$datetime,data5_rd5$Voltage)) + 
    geom_line() + scale_x_datetime(date_labels = "%a",date_breaks = "1 day") +
    ylab("Global Active Power") + xlab("Voltage")

g4<-ggplot(data5_rd5,aes(data5_rd5$datetime,data5_rd5$Global_reactive_power)) + 
    geom_line() + scale_x_datetime(date_labels = "%a",date_breaks = "1 day") +
    ylab("Global Active Power") + xlab("Global Reactive Power")


## Plot multiple graphs on the same grid. Need to install gridExtra.
grid.arrange(g1,g3,g2,g4,nrow=2)

}