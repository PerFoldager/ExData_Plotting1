## Course Project 1 in Coursera Exploratory Data Analysis
## The file file creates the first plot in the assignment.
## The function uses data that has been downloaded from here 
## URL="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
## The file household_power_consumption.txt has been extracted from the zip file and placed in data subfolder
## The file household_power_consumption.txt is input and the result of the processing is a histogram saved as PNG file plot2.png

plot2 <- function() {
  library(dplyr)
  library(timeDate)
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
  strptime(date_time, "%Y/%m/d %H:%M:%S")
  subset_df_power_con <- cbind(date_time, subset_df_power_con)
  row_count <- NROW(subset_df_power_con$Date)
  last_date <- as.Date(as.Date(subset_df_power_con$date_time[row_count]) + 1)
  first_date <- as.Date(subset_df_power_con$date_time[1])
  ## first_day <- dayOfWeek(as.timeDate(subset_df_power_con$date_time[1]))
  day_count <- as.numeric(last_date - first_date + 1)
  ## middle_date <- as.Date(first_date + (day_count / 2))
  day <- vector()
  position <- 0
  tick_pos <- vector()
  ## We set the positions of the first day on the x axis
  tick_pos[1] <- 1 
  for (i in 1:day_count) {
    day[i] <- dayOfWeek(as.timeDate(first_date + i - 1))  ## we need to subtract one to get the right date
    if (i > 1 & i < day_count) {
      ## we calculate the x axis tick positions between the first and the last by finding the row number of the date
      ## the day we look at is the previous day, so that we get first position of current day
      tick_pos[i] <- tick_pos[i - 1] + length(which(subset_df_power_con$Date == as.Date(first_date + i - 2) , arr.ind=TRUE))
    }
  }
  ## We set the positions of the last day on the x axis
  tick_pos[day_count] <- row_count

  ## Show the plot on the screen, just to make it quick and easy to see the result. Save to png done after the screen part.
  plot(subset_df_power_con$date_time, subset_df_power_con$Global_active_power, type="n", xaxt="n", ylab = "Global Active Power (kilowatts)", col = "black", border = "black")
  lines(subset_df_power_con$date_time, subset_df_power_con$Global_active_power)
  axis(1, at = tick_pos, labels = day, tick = TRUE)

  ## Save the plot as png file in working dir
  png(file = "plot2.png", bg = "transparent")
  plot(subset_df_power_con$date_time, subset_df_power_con$Global_active_power, type="n", xaxt="n", ylab = "Global Active Power (kilowatts)", col = "black", border = "black")
  lines(subset_df_power_con$date_time, subset_df_power_con$Global_active_power)
  axis(1, at = tick_pos, labels = day, tick = TRUE)
  dev.off()
  ## browser()
}  