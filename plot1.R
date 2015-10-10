## Course Project 1 in Coursera Exploratory Data Analysis
## The file file creates the first plot in the assignment.
## The function uses data that has been downloaded from here 
## URL="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
## The file household_power_consumption.txt has been extracted from the zip file and placed in data subfolder
## The file household_power_consumption.txt is input and the result of the processing is a histogram saved as PNG file plot1.png

plot1 <- function() {
  library(dplyr)
  ## Name data variables that will be used
  df_power_con <- data.frame()
  ## Read data into data frame
  df_power_con <- read.table("data/household_power_consumption.txt", header= TRUE, sep = ";", na.strings = "?", stringsAsFactors = FALSE)
  ## Transform the Date to a true date field so it is easier to select from
  df_power_con$Date <- as.Date(df_power_con$Date, "%d/%m/%Y")
  ## Create a subset that only has observartion from the two required dates
  subset_df_power_con = subset(df_power_con, Date >= '2007-02-01' & Date <= '2007-02-02')
  ## Create a new column that has a combined date and time field
  dates <- subset_df_power_con$Date
  times <- subset_df_power_con$Time
  date_time <- paste(dates, times)
  strptime(date_time, "%Y/%m/%d %H:%M:%S")
  subset_df_power_con <- cbind(date_time, subset_df_power_con)

  ## Show the plot on the screen, just to make it quick and easy to see the result. Save to png done after the screen part.
  hist(subset_df_power_con$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red", border = "black")

  ## Save the plot as png file in working dir
  png(file = "plot1.png", bg = "transparent")
  hist(subset_df_power_con$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red", border = "black")
  dev.off()
  ## browser()
}  