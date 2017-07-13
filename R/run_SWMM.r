## -------------------------------------------------------
## Andreas Scheidegger -- andreas.scheidegger@eawag.ch
## -------------------------------------------------------


##' Run SWMM simulations. If parameters are given the input file is modified accordingly.
##'
##' If the argument \code{parameters} is \code{NULL}, the inut file is used as is.
##' If \code{parameters} is a named vector the input file is treated as template, i.e. all
##' \code{\{parametername\}} are replaced with the value of \code{parameters}.
##' @title Run a SWMM simulation with given parameters
##' @param parameters \emph{named} vector of parameters or \code{NULL}
##' @param inputfile path/name to input file. If \code{parameters} is not \code{NULL}
##' the file is used as template, see Details below.
##' @param reportfilename path/name of the report file
##' @param outputfilename path/name of output file
##' @param SWMMexe path to SWMM executable
##' @param quite Boolean. If \code{FALSE} consol output is shown.
##' @return nothing
##' @author Andreas Scheidegger
##' @export
runSWMM <- function(parameters=NULL, inputfile,
                    reportfilename="report.rpt",
                    outputfilename="output.out",
                    SWMMexe="swmm5.exe", quite=FALSE){

  ## Change parameters in input file
  if(!is.null(parameters)){
    new.input <- tempfile()
    buildInputFile(inputfile=new.input, templatefile=inputfile, parameters)
    inputfile <- new.input
  }
  
  ## Run SWMM 
  inputfile <- paste0('"', normalizePath(inputfile, mustWork=TRUE),'"')
  reportfile <- paste0('"', normalizePath(reportfilename, mustWork=FALSE),'"')
  outputfile <- paste0('"', normalizePath(outputfilename, mustWork=FALSE),'"')
  SWMMexe <- paste0('"', normalizePath(SWMMexe, mustWork=TRUE),'"')

  command <- paste(SWMMexe, inputfile, reportfile, outputfile, sep=" ")

  if(!quite){
    cat("\nExecuting:\n ", command)
    if(.Platform$OS.type == "unix"){
      system(command)
    } else {
      system(command, show.output.on.console=TRUE)
    }
  } else {
    system(command)
  }
  flush.console()

}



buildInputFile <- function(inputfile, templatefile, parameters){

  SWMMTemplate <- readLines(templatefile, n=-1, ok=TRUE, warn=TRUE,
                            encoding="unknown")

  if(is.null(parameters)) stop("'parameters' must be a named vector!")
  
  ## replaces {parametername} the input file with parameter
  n.matches <- rep(NA, length(parameters))
  for(i in seq_along(parameters)){
    pattern <- paste0("\\{", names(parameters)[i], "\\}")
    n.matches[i] <- sum(sapply(gregexpr(pattern, SWMMTemplate), function(x) sum(x[1]>0)))
    SWMMTemplate <- gsub(pattern, parameters[i], SWMMTemplate)
  }

  ## check for templates that are not found in parameters
  missing.matches <- regmatches(SWMMTemplate, gregexpr("\\{[^\\\\{]*\\}", SWMMTemplate))
  n.missing <- sum(sapply(missing.matches, function(x) length(x)>0))
  if(n.missing>0) {
    error.str <- cat("\nThe following templates are found in file \n  '",
                     templatefile, "'\n but not in 'parameters':\n", sep="")
    for(m in seq_along(missing.matches)){
      if(length(missing.matches[[m]])>0)
        error.str <- cat(error.str, "  Line ", m, ": ",
                         paste(missing.matches[[m]], collapse=", "), "\n", sep="")
    } 
    stop(error.str, call. = TRUE)
  }

  ## check of parameters that are not found in template
  if(any(n.matches==0)){
    stop(paste("\nThe follwing parameter(s) could not be found in the template file:\n-  "),
         paste(names(parameters)[n.matches==0], collapse="\n-  "))
  }

  ## write input file
  writeLines(SWMMTemplate, con=inputfile, sep = "\n", useBytes = FALSE)
}



