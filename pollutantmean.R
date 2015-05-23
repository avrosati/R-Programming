pollutantmean <- function(directory, pollutant, id = 1:332){
  ## directory = character vector of length 1; location of CSV files
  ## pollutant = charafter vector of length 1; name of pollutant (either "sulfate" or "nitrate")
  ## id = integer vector indicating monitor ID numbers to be used.
  ## Returns the mean of the pollutant across all monitors list in the ID vector (ignoring all NA values)
  ## NO ROUNDING!
  
  ## create filelist for data loading...
  files2read <- list.files(directory, pattern=".csv",full.names = TRUE)
  ## Read all the raw data into a dataframe...
  dataRaw <- do.call(rbind,lapply(files2read,read.csv))
  ## create complete cases data frame...
  completeCases <- complete.cases(dataRaw)
  goodCases <- dataRaw[completeCases, ]
  ## create "requested" (using id vector) complete cases data frame...
  goodCasesSub <- subset(goodCases,ID %in% id)
  ## Calculate the mean of the pollutant requested ...
  mean(goodCasesSub[,pollutant]) 
}