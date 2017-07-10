## -------------------------------------------------------
## Andreas Scheidegger -- andreas.scheidegger@eawag.ch
## -------------------------------------------------------



runSWMM <- function(parameters=NULL, inputfile, reportfile="report.rpt",
                    outputfile="output.out",
                    SWMMexe='swmm5.exe', verbose=TRUE){

  ## Change parameters in input file

  replacementCodes <- ""
  inp <- tempfile()
  createInputFile(inp, inputfile, parameters, replacementCodes)
  inputfile <- inp
  
  ## Run SWMM 
  inputfile <- paste0('"', inputfile,'"')
  reportfile <- paste0('"', reportfile,'"')
  outputfile <- paste0('"', outputfile,'"')
  SWMMexe <- paste0('"', SWMMexe,'"')

  command <- paste(SWMMexe, inputfile, reportfile, outputfile, sep=" ")

  if(verbose){
    print(paste("Executing: ",command,sep="",collapse=""))
    system(command, show.output.on.console=T)
  } else {
    system(command, show.output.on.console=F)
  }
  flush.console()

}



createInputFile <- function(inputfile, templatefile, parameters, replacementCodes){

  SWMMTemplate <- readLines(templatefile, n=-1, ok=TRUE, warn=TRUE,
                            encoding="unknown")
  unlink(templatefile)
  
  ## replaces the codes in an input file with parameters from optimization
  for(i in 1:length(parameters)){
    SWMMTemplate <- gsub(replacementCodes[i], parameters[i], SWMMTemplate,fixed=TRUE)
  }

  ## ???
  SWMMTemplate <- gsub("//", "////", SWMMTemplate, ignore.case = FALSE, perl = FALSE,
                       fixed <- FALSE, useBytes = FALSE)
  writeLines(SWMMTemplate, con=inputfile, sep = "\n", useBytes = FALSE)

}



