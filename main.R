
source("prepData.R")

require("mice")

options(warn = 2)


try(dataset <- readLocalCSVFile("activity.zip", unzip = TRUE), silent=TRUE)

## The data set contains measurements taken at 5 minute intervals for the months of October and November
## Expected readings

expectedReadings <- (61 * 24 * 60) / 5

num_rows <- nrow(dataset)

## Verify that we have the expected number of readings
## if not equal, need to figure out a scheme for imputing
if(num_rows < expectedReadings) {
  print("Need to add rows")
} else if(num_rows > expectedReadings) {
  print("Excess rows detected")
} else {
  print("Expected number of data points detected.")
}

## Indentify NA fields 
na_steps <- which(is.na(dataset$steps))
na_date <- which(is.na(dataset$date))
na_interval <- which(is.na(dataset$interval))

totNARows <- union(union(na_steps, na_date), na_interval)



imputed <- mice(dataset)

completeImputed <- complete(imputed)

View(imputed)
View(dataset)
