## -------------------------------------------------------
## Andreas Scheidegger -- andreas.scheidegger@eawag.ch
## -------------------------------------------------------



## runSWMM <- function(inpFile,rptFile,outFile,SWMMexe='swmm5.exe',verbose=T){
##   ## runs swmm on the inpFile, rptFile, outFile provided.  Does not create directoies
##   inpFile <- paste('"', inpFile,'"',sep="")
##   rptFile <- paste('"', rptFile,'"',sep="")
##   outFile <- paste('"', outFile,'"',sep="")
##   SWMMexe <- paste('"',SWMMexe,'"',sep="")

##   command <- paste(SWMMexe,inpFile,rptFile,outFile,sep=" ")

##   if(verbose){
##     print(paste("Executing: ",command,sep="",collapse=""))
##     system(command,show.output.on.console=T)
##   }else{
##     system(command,show.output.on.console=F)
##   }
##   flush.console()

## }



## getCalDataFromCSV <- function(CSVFile,dateFormat="%m/%d/%y %H:%M"){
##   ## Calibration Data should be in a CSV with datetimes in the first column,
##   ## and data in the second column
##   ## The text file is assumed to have a one line header
##   ## Call this function with the correct dateFormat for your datetimes
##   ## the dateFormat is passed to strptime, so look for formatting information there
##   ## for example, dates like this 1/1/07 12:00, can be read with the default dateFormat

##   temp=read.csv(file=CSVFile, header = TRUE, sep = ",", quote="\"", dec=".",
##                 fill <- TRUE, comment.char="",stringsAsFactors = FALSE)
##   calData <- {}
##   calData$times <- as.POSIXct(strptime(temp[,1], format=dateFormat,tz="GMT")) # edit 2/10/2012 to force GMT time zone rather than locale specific
##   calData$obs <- temp[,2]
##   return(calData)

## }


## interpCalDataToSWMMTimes <- function(headObj){
##   ## this linearly interpolates observations for calibration onto the time stamps of the SWMM model
##   calData$interpedObs <- approx(calData$times, calData$obs, headObj$SWMMTimes,
##                                 method="linear",
##                                 yleft <- NaN, yright=NaN, rule = 1)$y

## }


## readTemplateFile <- function(SWMMTemplateFile){
##   ## reads in the SWMM file that has replacement codes
##   SWMMTemplate=readLines(con = SWMMTemplateFile, n = -1L, ok = TRUE, warn = TRUE,
##                          encoding = "unknown")
##   return(SWMMTemplate)
## }

## replaceCodesInTemplateFile <- function(SWMMTemplate,parameters,replacementCodes){
##   ## replaces the codes in an input file with parameters from optimization
##   for(i in 1:length(parameters)){

##     SWMMTemplate <- gsub(replacementCodes[i], parameters[i], SWMMTemplate,fixed=TRUE)
##   }
##   return(SWMMTemplate)
## }

## writeNewInputFile <- function(SWMMTemplate,filename){
##   ## writes a new input file after replacement codes have been replaced by parameters
##   SWMMTemplate=gsub("//", "////", SWMMTemplate, ignore.case = FALSE, perl = FALSE,
##                     fixed <- FALSE, useBytes = FALSE)
##   writeLines(SWMMTemplate, con = filename, sep = "\n", useBytes = FALSE)

## }

