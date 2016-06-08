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
debug(readLocalCSVFile)

## - missing input file
try(res <- readLocalCSVFile("missing_input_file.csv"), silent=TRUE)
assert_that(!exists("res"))

## - empty csv input file
try(res <- readLocalCSVFile("empty_input_file.csv"), silent=TRUE)
assert_that(!exists("res"))

## - valid csv input file
try(res <- readLocalCSVFile("valid_input_file.csv"), silent=TRUE)
assert_that(exists("res") && is.data.frame(res))



####
## cleanup. 
## TODO Need to restore the workspace
setwd(PREV_WD);

