complete <- function(directory, id=1:332) {
  ## directory = character vector of length 1; location of CSV files
  ## id = integer vector; monitor ID numbers to be used
  ## Returns a dataframe of the form:
  ##     id     nobs
  ##      1     117
  ##      2    1041
  ##     ...
  ##  id = Monitor ID number
  ##  nobs = number of complete cases
  
  ## create filelist for data loading...
  files2read <- list.files(directory, pattern=".csv",full.names = TRUE)
  ## Read all the raw data into a dataframe...
  dataRaw <- do.call(rbind,lapply(files2read,read.csv))
  ## create complete cases data frame...
  completeCases <- complete.cases(dataRaw)
  goodCases <- dataRaw[completeCases, ]
  ## create "requested" (using id vector) complete cases data frame...
  goodCasesSub <- subset(goodCases,ID %in% id)
  ## Determine the number of complete observations for each Monitor
  completeObs <- data.frame()
  for (i in id){
    nobs = nrow(subset(goodCasesSub, ID == i))
    completeObs = rbind(completeObs, c(i,nobs))
  }
  colnames(completeObs) <- c("ID","nobs")
  completeObs
}