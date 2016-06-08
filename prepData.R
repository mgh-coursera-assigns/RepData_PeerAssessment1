##
##  Coursera/Johns Hopkins 
##      Data Science Specialization
##      Reproducible Research
##      June , 2016
##  Project 1 
##
##  



##
##  Loads a dataframe from a CSV file.
##    If specified by 'unzip' assumes 'localFile' is a zip archive 
##    and extracts its contents into 'unzipDestDir' or the 
##    current working directory if 'unzipDestDir' is empty or NULL.
##    This function assumes that the zip file contains a single CSV file.
##
readLocalCSVFile <- function(localFile, unzip = FALSE, unzipDestDir = NULL) {
  
  require("stringr");
  
  if( !file.exists(localFile)) {
    warning(paste0("File \'", localFile, "\'. Check permissions and access privileges if the file actually exists."));
    return;
  }
 
  retVal = localFile;
  if(unzip) { 
    if(is.null(unzipDestDir) || isEmpty(unzipDestDir)) {
      unzipDestDir <- getwd();  
    }
    
    unzipDestDir <- str_trim(unzipDestDir);
    
    if(!file.exists(unzipDestDir) 
        && !file.create(unzipDestDir)) {
      warning(paste0("Unable to create unzip destination directory \'", unzipDestDir, "\'"));
    }

    retVal = unzip(localFile, exdir = unzipDestDir);
    ## Put error handler on failure to unzip
  }  
  
  if(!is.null(retVal)) {
    retVal <- read.csv(retVal, header=TRUE)
  }
  
  return( retVal );
}
