##
##  Rudimentary unit tests for functions in prepData.R
##
##


## clear workspace.
## TODO Save workspace for restoring at end of test
rm(list=ls())

## set working directory etc.
NEW_WD <- paste0("/Users/meghadri/Documents/Coursera/",
                  "ReproducibleResearch-JHU-RogerPeng-20150530/Course Project 1/",
                  "Submission/RepData_PeerAssessment1/unittests");
PREV_WD <- setwd(NEW_WD);
### print out env

##print(paste0("Working directory for tests: \'", getwd(), "\'"))


source("../prepData.R")

require("assertthat")
options(warn = 2)

#####
## Test readLocalCSVFile(..)
##debug(readLocalCSVFile)

## - missing input file
try(res <- readLocalCSVFile("missing_input_file.csv"), silent=TRUE)
try(assert_that(!exists("res")), stop("unable to read file missing_input_file.csv"))

##try(assert_that(exists("res") && is.data.frame(res)), stop(paste0("Unable to load data from \'activity.zip\'. ", "Current working directory: ", getwd())))
##tail(warnings(), 1)
##try(assert_that(exists("res") && is.data.frame(res)), stop(paste0("Unable to load data from** \'activity.zip\'. ", "Current working directory: ", getwd(), " ", tail(warnings(), 1))))

## - empty csv input file
try(res <- readLocalCSVFile("empty_input_file.csv"), silent=TRUE)
assert_that(!exists("res"))

## - valid csv input file
try(res <- readLocalCSVFile("valid_input_file.csv"), silent=TRUE)
assert_that(exists("res") && is.data.frame(res))

##require("knitr")
head(res)

## View(res)

res$date <- factor(res$date)
levels(res$date)

res2 <- split(res, res$date)
head(res2)

df_DailySteps <- data.frame(date = character(length(res2)), steps = integer(length(res2)), stringsAsFactors = FALSE)
totalTimeEachDay <- list()

iter <- 0
for (day in res2) {
      sumDay <- sum(day$steps, na.rm = TRUE);
      totalTimeEachDay <- append(totalTimeEachDay, sumDay)
      ##newR <- c(as.character(day$date[1]), as.numeric(sumDay))
      iter <- iter + 1
      df_DailySteps$date[iter] <- as.character(day$date[1])
      df_DailySteps$steps[iter] <- as.numeric(sumDay)

      ##print(paste0("sumDay = ", sumDay))
}

##df_DailySteps <- as.data.frame(totalTimeEachDay)

days <- seq(df_DailySteps$date)
head(df_DailySteps)

meanTotalDailySteps <- mean(as.numeric(df_DailySteps$steps), na.rm = TRUE)
medianTotalDailySteps <- median(as.numeric(df_DailySteps$steps), na.rm = TRUE)

require("ggplot2")

##qplot(df_DailySteps$date, geom = "histogram", binwidth = 0.5, xlab = "Day", col = I("red"), main = "Histogram")
p <- ggplot(data = df_DailySteps, aes(x = date))
p <- p + geom_bar(aes(y = steps), stat = "identity", alpha = 1, width = 0.6, position = position_dodge(width = 1.9))
p <- p + theme(axis.text.x = element_text(angle = 90))
p <- p + xlab("Dates") + ylab("Total Steps") + ggtitle("Daily Total Steps")

print(p)
##ggplot(df_DailySteps, aes(as.numeric(df_DailySteps$date), df_DailySteps$steps)) + geom_histogram()

factIntvlData <- split(res, res$interval)
df_IntvlSteps <- data.frame(interval = integer(length(factIntvlData)), steps = integer(length(factIntvlData)), stringsAsFactors = FALSE)

iter <- 0
for (intvl in factIntvlData) {
      sumIntvl <- sum(intvl$steps, na.rm = TRUE);
      ##totalTimeEachDay <- append(totalTimeEachDay, sumDay)
      ##newR <- c(as.character(day$date[1]), as.numeric(sumDay))
      iter <- iter + 1
      df_IntvlSteps$interval[iter] <- as.numeric(intvl$interval[1])
      df_IntvlSteps$steps[iter] <- as.numeric(sumIntvl)

      ##print(paste0("sumDay = ", sumDay))
}

##p <- ggplot(df_IntvlSteps, aes(x = interval)) + geom_line(aes( y = steps), stat = "identity")
##print(p)
xrange <- range(df_IntvlSteps$interval)
yrange <- range(df_IntvlSteps$steps)
seqX <- seq(1, length(df_IntvlSteps$interval))
plot(seqX, df_IntvlSteps$steps, type="l", main="Total Steps by Interval", xlab = "Interval", ylab = "Steps", xaxt='n')

seqXTicks <- seq(0, length(df_IntvlSteps$interval), by = 4)
axis(side = 1, at = seqXTicks)

maxPer <- which.max(df_IntvlSteps$steps)
perWithMaxSteps <- df_IntvlSteps[which.max(df_IntvlSteps$steps), ]$interval

clockTimeAtIntvl <- paste0(perWithMaxSteps %/% 60, ":", round((perWithMaxSteps %% 60) * 60, 2), " hours")

##plot(range(df_IntvlSteps$interval), range(df_IntvlSteps$steps), type = "n", xlab = "Interval", ylab = "Total Steps")
##lines(df_IntvlSteps$interval, df_IntvlSteps$steps, type = "b", lwd = "1")

View(df_IntvlSteps)
####
## cleanup.
## TODO Need to restore the workspace
setwd(PREV_WD)
