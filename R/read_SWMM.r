## -------------------------------------------------------
## Andreas Scheidegger -- andreas.scheidegger@eawag.ch
## -------------------------------------------------------

##' opens a binay SWMM output file
##'
##' @title Open a SWMM output file
##' @param SWMMoutfile character, path to output file
##' @param timezone time zone specification of the dates. If \code{""}, the local time zone
##' is used.
##' @return a \code{SWMMfile} object
##' @author Andreas Scheidegger
##' @export
openSWMMOutput <- function(SWMMoutfile, timezone="") {
  
  RECORDSIZE <- 4 
  f <- file(SWMMoutfile, "rb")

  f.props <- list()
  
  ## ---
  ## read the header of a binary output file

  header <- readBin(f, integer(), n=7, size=4)
  
  if(header[1] != 516114522){
    stop(paste0("'", SWMMoutfile,"' is not a valid SWMM file!"))
  }

  f.props$SWMMversion <- header[2]
  if(f.props$SWMMversion != 51000){
    stop("SWMM V5.1 is required!")
  }
  
  f.props$numSubc <- header[4]
  f.props$numNode <- header[5]
  f.props$numLink <- header[6]
  f.props$numPoll <- header[7]
 
  ## Units: 0 = CFS, 1 = GPM, 2 = MGD, 3 = CMS, 4 = LPS, and 5 = LPD
  flowUnits <- c("cubic feet per second", "US gallons per minute",
                 "million US gallons per day", "cubic meters per second",
                 "liters per second", "liters per day")
  f.props$flowUnit <- flowUnits[header[3]+1]
  
  seek(f,-6*4,"end")
  f.props$position.objectID <- readBin(f, integer(), n = 1, size = 4)
  f.props$position.objectF.Props <- readBin(f, integer(), n = 1, size = 4)
  f.props$position.computedResults <- readBin(f, integer(), n = 1, size = 4)
  f.props$numReportingPeriods <- readBin(f, integer(), n = 1, size = 4)
  f.props$errorStatus <- readBin(f, integer(), n = 1, size = 4)
  
  if(f.props$errorStatus > 0){
    stop(paste("SWMM error! Error code: ", f.props$errorStatus))
  }

  ## ---
  ## object names
  
  seek(f, f.props$position.objectID, "start");
  
  ## For all subcatchments
  f.props$subcNames <- list()
  if(f.props$numSubc > 0){
    for(i in 1:f.props$numSubc){
      lengthName <- readBin(f, integer(), n = 1, size = 4)
      f.props$subcNames[i] <- readChar(f, lengthName, useBytes = FALSE)
    }
  }
  
  ## For all nodes
  f.props$nodeNames <- list()
  if(f.props$numNode > 0){
    for(i in 1:f.props$numNode){
      lengthName <- readBin(f, integer(), n = 1, size = 4)
      f.props$nodeNames[i] <- readChar(f, lengthName, useBytes = FALSE)
    }
  }
  ## For all links
  f.props$linkNames <- list()
  if(f.props$numLink > 0) {
    for(i in 1:f.props$numLink){
      lengthName <- readBin(f, integer(), n = 1, size = 4)
      f.props$linkNames[i] <- readChar(f, lengthName, useBytes = FALSE)
    }
  }
  ## For all pollutants
  f.props$pollNames <- list()
  f.props$pollUnits <- list()
  if(f.props$numPoll > 0) {
    for(i in 1:f.props$numPoll){
      lengthName <- readBin(f, integer(), n = 1, size = 4)
      f.props$pollNames[i] <- readChar(f, lengthName, useBytes = FALSE)
    }
    for(i in 1:f.props$numPoll){
      unitCode <- readBin(f, integer(), n = 1, size = 4)
      if(unitCode==0){
        f.props$pollUnits[i] <- "mg/L"
      } else if(unitCode==1){
        f.props$pollUnits[i] <- "ug/L"
      } else if(unitCode==2){
        f.props$pollUnits[i] <- "counts/L"
      }
    }
  }
  
  seek(f, f.props$position.objectF.Props, "start")


  ## ---
  ## object properties
  
  ## Subcatchments
  f.props$numSubcPropSaved <- readBin(f, integer(), n = 1, size = 4)
  f.props$codesSubcPropSaved <- readBin(f, integer(), n = 1, size = 4)
  
  if (f.props$codesSubcPropSaved==1){
    f.props$subcArea <- readBin(f ,what="double", n=f.props$numSubc,size=4)
  }

  ## Nodes
  f.props$numNodePropSaved <- readBin(f, integer(), n = 1, size = 4)
  f.props$codesNodePropSaved <- readBin(f, integer(),n=f.props$numNodePropSaved,size=4)

  if(f.props$numNode>0){
    temp <- readBin(f,"integer", n=f.props$numNodePropSaved*f.props$numNode, size=4)
    codestemp <- temp[seq.int(from=1,by=3,to=length(temp))]
    f.props$nodeType <- list()
    count <- 0
    for(i in codestemp) {
      count <- count+1
      if(i==0){
        f.props$nodeType[count] <- "Junction"
      } else if(i==1){
        f.props$nodeType[count] <- "Outfall"
      } else if(i==2){
        f.props$nodeType[count] <- "Storage"
      } else if(i==3){
        f.props$nodeType[count] <- "Divider"
      }
    }
    f.props$nodeInvertElevation <- temp[seq.int(from=2,by=3,to=length(temp))]
    f.props$nodeMaxDepth <- temp[seq.int(from=3,by=3,to=length(temp))]
  }


  ## Links
  f.props$numLinkPropSaved <- readBin(f, integer(), n = 1, size = 4)
  f.props$codesLinkPropSaved <- readBin(f, integer(), n =f.props$numLinkPropSaved, size = 4)

  if(f.props$numLink>0){
    temp <- readBin(f,what="integer",n=f.props$numLinkPropSaved*f.props$numLink,size=4)
    codestemp <- temp[seq.int(from=1,by=5,to=length(temp))]
    f.props$linkType <- list()
    count <- 0
    for(i in codestemp){
      count <- count+1
      if(i==0){
        f.props$linkType[count]="Conduit"
      }else if(i==1){
        f.props$linkType[count]="Pump"
      }else if(i==2){
        f.props$linkType[count]="Orifice"
      }else if(i==3){
        f.props$linkType[count]="Weir"
      }else if(i==4){
        f.props$linkType[count]="Outlet"
      }
    }

    f.props$linkUpstreamInvertOffset <- temp[seq.int(from=2,by=5,to=length(temp))]
    f.props$linkDownstreamInvertOffset <- temp[seq.int(from=3,by=5,to=length(temp))]
    f.props$linkMaxDepth <- temp[seq.int(from=4,by=5,to=length(temp))]
    f.props$linkLength <- temp[seq.int(from=5,by=5,to=length(temp))]
    
  }

  ## ---
  ## reporting variables
  
  f.props$numSubcVars <- readBin(f, integer(), n = 1, size = 4)
  f.props$subcVarCodes <- readBin(f,integer(),n = f.props$numSubcVars,size=4)
  
  f.props$numNodeVars <- readBin(f, integer(), n = 1, size = 4)
  f.props$nodeVarCodes <- readBin(f,integer(), n = f.props$numNodeVars,size=4)
  
  f.props$numLinkVars <- readBin(f, integer(), n = 1, size = 4)
  f.props$linkVarCodes <- readBin(f,integer(), n = f.props$numLinkVars,size=4)
  
  f.props$numSysVars <- readBin(f,integer(), n = 1, size = 4)
  f.props$sysVarCodes <- readBin(f,integer(), n = f.props$numSysVars,size=4)

  f.props$bytesPerPeriod <- 2*RECORDSIZE +
    RECORDSIZE * (f.props$numSubc * f.props$numSubcVars +
                  f.props$numNode * f.props$numNodeVars +
                  f.props$numLink * f.props$numLinkVars +
                  f.props$numSysVars)

  ## ---
  ## read out time stamps
  
  seek(f, f.props$position.computedResults, "start")

  f.props$SWMMTimes <- array(NA, f.props$numReportingPeriods)
  if(f.props$numReportingPeriods > 0){
    for(i in 1:f.props$numReportingPeriods){
      f.props$SWMMTimes[i] <- readBin(f,what="double",size=8,n=1)
      seek(f, f.props$bytesPerPeriod-8, "current")
    }
  } else {
    stop("No time steps listed in SWMM file.")
  }
  
  ## Convert times to POSIXlt datetimes
  f.props$SWMMTimes <- f.props$SWMMTimes*86400.0 +
    as.POSIXct(strptime("12/30/1899", format="%m/%d/%Y", tz=timezone))

  ## add file handler
  f.props$outFileHandle <- f
  f.props$fileName <- normalizePath(SWMMoutfile)
  class(f.props) <- "SWMMfile"
  return(f.props)
}


##' Closes file connection of a \code{SWMMfile} object.
##' @title Closes a SWMM file handler
##' @param SWMMfile a \code{SWMMfile} object
##' @return nothing
##' @author Andreas Scheidegger
##' @export
closeSWMMOutput <- function(SWMMfile){
  close(SWMMfile$outFileHandle)
} 


## ' Print function for \code{SWMMfile} objects.
## ' @title Print main properties of a SWMM output file
## ' @param SWMMfile a \code{SWMMfile} object
## ' @return nothing


##' @author Andreas Scheidegger
##' @export
print.SWMMfile <- function(x, ...){

  cat(paste0("Connection to SWMM output file:\n   ", x$fileName, "\n\n"))

  cat("Simulation from", as.character(min(x$SWMMTimes)), "to",
      as.character(max(x$SWMMTimes)), "\n")
  dt <- diff(x$SWMMTimes[1:2])
  cat(" in ", length(x$SWMMTimes), " time steps \u00E0 ", dt, attr(dt, "unit"),
      ".\n\n", sep="")

  cat("Flow unit is ", x$flowUnit, ".\n\n", sep="")
  
  cat(length(x$subcNames), "subcatchments")
  if(length(x$subcNames)>0){
    cat(":\n-  ")
    cat(paste(unlist(x$subcNames), collapse="\n-  "))
  }

  cat("\n\n")
  cat(length(x$nodeNames), "nodes")
  if(length(x$nodeNames)>0) {
    cat(":\n-  ")
    cat(paste(paste0(unlist(x$nodeNames),
                     " (",unlist(x$nodeType), ")"), collapse="\n-  "))
  }

  cat("\n\n")
  cat(length(x$linkNames), "links")
  if(length(x$linkNames)>0) {
    cat(":\n-  ")
    cat(paste(unlist(x$linkNames), collapse="\n-  "))
  }

  cat("\n\n")
  cat(x$numPoll, "pollutants")
  if(x$numPoll>0){
    cat(":\n-  ")
    cat(paste(paste0(unlist(x$pollNames),
                     " [",unlist(x$pollUnits), "]"), collapse="\n-  "))
  }

  cat("\n\n")

}



##' This function used internally only. Reads a single result from a SWMM outputfile.
##'
##' 
##'\code{iType} should be 0 for Subcatchments, 1 for nodes, 2 for links, or 3 for system variables.
##' 
##' \code{vIndex} should be selected from this lists below, depending on whether \code{iType} is a subcatchment, a link, or a node.
##'                        
##' Number of subcatchment variables (currently 8 + number of pollutants).
##' Code number of each subcatchment variable:
##' 0 for rainfall (in/hr or mm/hr),
##' 1 for snow depth (in or mm),
##' 2 for evaporation losses (in/hr or mm/hr),
##' 3 for infiltration losses (in/hr or mm/hr),
##' 4 for runoff rate (flow units),
##' 5 for groundwater outflow rate (flow units),
##' 6 for groundwater water table elevation (ft or m),
##' 7 for unsaturated zone moisture content,
##' 8 for runoff concentration of first pollutant,
##' 8 + N for runoff concentration of N-th pollutant.
##' 
##' Number of node variables (currently 6 + number of pollutants)
##' Code number of each node variable:
##' 0 for depth of water above invert (ft or m),
##' 1 for hydraulic head (ft or m),
##' 2 for volume of stored + ponded water (ft3 or m3),
##' 3 for lateral inflow (flow units),
##' 4 for total inflow (lateral + upstream) (flow units),
##' 5 for flow lost to flooding (flow units),
##' 6 for concentration of first pollutant,
##' 5 + N for concentration of N-th pollutant.
##' 
##' Number of link variables (currently 5 + number of pollutants)
##' Code number of each link variable:
##'                            
##' 0 for flow rate (flow units),
##' 1 for flow depth (ft or m),
##' 2 for flow velocity (ft/s or m/s),
##' 3 for Froude number,
##' 4 for capacity (fraction of conduit filled),
##' 5 for concentration of first pollutant,
##' 4 + N for concentration of N-th pollutant.
##' 
##' Number of system-wide variables (currently 15)
##' Code number of each system-wide variable:
##' 0 for air temperature (deg. F or deg. C),
##' 1 for rainfall (in/hr or mm/hr),
##' 2 for snow depth (in or mm),
##' 3 for evaporation + infiltration loss rate (in/hr or mm/hr),
##' 4 for runoff flow (flow units),
##' 5 for dry weather inflow (flow units),
##' 6 for groundwater inflow (flow units),
##' 7 for RDII inflow (flow units),
##' 8 for user supplied direct inflow (flow units),
##' 9 for total lateral inflow (sum of variables 4 to 8) (flow units),
##' 10 for flow lost to flooding (flow units),
##' 11 for flow leaving through outfalls (flow units),
##' 12 for volume of stored water (ft3 or m3),
##' 13 for evaporation rate (in/day or mm/day)
##' 14 Daily potential evapotranspiration (PET) (in/day or mm/day)
##' 
##' @title Read a single SWMM result
##' @param SWMMfile a \code{SWMMfile} object
##' @param iType \code{0}, \code{1}, \code{2}, or \code{3}. See Detail below.
##' @param iIndex integer, the position of the subcatch, subcatch, node, link or sys object
##' @param vIndex integer, index of the variable. See Detail below.
##' @param period integer, index of the period (time step)
##' @return a scalar
##' @author Andreas Scheidegger
readSingleResult <- function(SWMMfile, iType, iIndex, vIndex, period){

  RECORDSIZE <- 4
  f <- SWMMfile$outFileHandle
  
  StartPos <- SWMMfile$position.computedResults
  offset <- StartPos + period*(SWMMfile$bytesPerPeriod) + 2*RECORDSIZE
  
  if(iType == 0){                       # subcatchment
    offset <- offset + RECORDSIZE*(iIndex*(SWMMfile$numSubcVars) + vIndex)
  } else if(iType == 1){                # Node
    offset <- offset + RECORDSIZE*(SWMMfile$numSubc*(SWMMfile$numSubcVars) +
                                   iIndex*(SWMMfile$numNodeVars) + vIndex)
  } else if(iType == 2){                # link
    offset <- offset+ RECORDSIZE*(SWMMfile$numSubc*(SWMMfile$numSubcVars) +
                                  SWMMfile$numNode*(SWMMfile$numNodeVars) +
                                  iIndex*(SWMMfile$numLinkVars) + vIndex)
  } else if(iType == 3){                # system
    offset <- offset + RECORDSIZE*(SWMMfile$numSubc*(SWMMfile$numSubcVars) +
                                   SWMMfile$numNode*(SWMMfile$numNodeVars) +
                                   SWMMfile$numLink*(SWMMfile$numLinkVars) + vIndex)
  }

  seek(f, offset, "start")
  output <- readBin(f, what="double", size=4, n=1)
  return(output)
}


##' Read subcatchment results from a SWMM file.
##'
##' The following variables are available for subcatchments:
##' \itemize{
##' \item{"rainfall"}{}
##' \item{"snow depth"}{}
##' \item{"evaporation losses"}{}
##' \item{"infiltration losses"}{}
##' \item{"runoff"}{}
##' \item{"groundwater outflow"}{}
##' \item{"groundwater water table elevation"}{}
##' \item{"unsaturated zone moisture content"}{fraction}
##' \item{"PollutantName"} {Only if pollutions are modeled.}
##' }
##' @title read SWM results for subcatchments
##' @param SWMMfile a \code{SWMMfile} object
##' @param names vector of names of the subcatchment to be read. Partial matching is supported.
##' @param variables vector of variables to be read for each subcatchment, see Details below. If \code{NULL}, all variables are read. Partial matching is supported.
##' @return A named list of separate \code{xts} objects for each variable.
##' The \code{xts} objects contain the data for all subcatchment.
##' @author Andreas Scheidegger
##' @export
readSubcatchments <- function(SWMMfile, names, variables=NULL) {

  ## -- get index of elements
  iIndexes <- pmatch(names, SWMMfile$subcNames) - 1
  if(any(is.na(iIndexes))){
    stop(paste0("The following subcatchment names(s) could not be found in the file:\n-  ",
                paste(names[is.na(iIndexes)], collapse="\n-  ")))
  }

  ## -- get variable index
  vars <- c("rainfall", "snow depth", "evaporation losses",
            "infiltration losses", "runoff", "groundwater outflow",
            "groundwater water table elevation", "unsaturated zone moisture content",
            unlist(SWMMfile$pollNames))
  
  if(is.null(variables)) {
    vIndexes = 0:(length(vars)-1)
  } else {
    vIndexes <- pmatch(variables, vars) - 1
  }

  if(any(is.na(vIndexes))){
    stop(paste0("The following variable(s) could not be uniquely matched:\n-  ",
                paste(variables[is.na(vIndexes)], collapse="\n-  ")))
  }

    
  ## read 
  results <- list()
  for(v in vIndexes){
    temp <- matrix(NA, ncol=length(iIndexes), nrow=SWMMfile$numReportingPeriods)
    colnames(temp) <- names 
    for(i in seq_along(iIndexes)){
      for(period in 1:SWMMfile$numReportingPeriods){
        temp[period,i] <- readSingleResult(SWMMfile, iType=0, iIndex=iIndexes[i],
                                           vIndex=v, period=period-1) 
      }
    }
    results[[vars[v+1]]] <- xts(temp, SWMMfile$SWMMTimes)
  }

  results
}




##' Read node results from a SWMM file.
##'
##' The following variables are available for nodes:
##' \itemize{
##' \item{"depth of water above invert"}{}
##' \item{"hydraulic head"}{}
##' \item{"volume of stored + ponded water"}{}
##' \item{"lateral inflow"}{}
##' \item{"total inflow"}{lateral + upstream}
##' \item{"flow lost to flooding"}{}
##' \item{"PollutantName concentration"} {Only if pollutions are modeled.}
##' }
##' @title read SWM results for nodes
##' @param SWMMfile a \code{SWMMfile} object
##' @param names vector of names of the nodes to be read. Partial matching is supported.
##' @param variables vector of variables to be read for each node, see Details below. If \code{NULL}, all variables are read. Partial matching is supported.
##' @return A named list of separate \code{xts} objects for each variable.
##' The \code{xts} objects contain the data for all nodes.
##' @author Andreas Scheidegger
##' @export
readNodes <- function(SWMMfile, names, variables=NULL){

  ## -- get index of elements
  iIndexes <- pmatch(names, SWMMfile$nodeNames) - 1
  if(any(is.na(iIndexes))){
    stop(paste0("The following node names(s) could not be found in the file:\n-  ",
                paste(names[is.na(iIndexes)], collapse="\n-  ")))
  }

  ## -- get variable index
  vars <- c("depth of water above invert", "hydraulic head", "volume of stored + ponded water",
            "lateral inflow", "total inflow", "flow lost to flooding")
  if(length(SWMMfile$pollNames)>0){
    vars <- c(vars, paste(unlist(SWMMfile$pollNames), "concentration"))
  }
  
  if(is.null(variables)){
    vIndexes = 0:(length(vars)-1)
  } else {
    vIndexes <- pmatch(variables, vars) - 1
  }

  if(any(is.na(vIndexes))){
    stop(paste0("The following variable(s) could not be uniquely matched:\n-  ",
                paste(variables[is.na(vIndexes)], collapse="\n-  ")))
  }
  
  ## read 
  results <- list()
  for(v in vIndexes){
    temp <- matrix(NA, ncol=length(iIndexes), nrow=SWMMfile$numReportingPeriods)
    colnames(temp) <- names 
    for(i in seq_along(iIndexes)){
      for(period in 1:SWMMfile$numReportingPeriods){
        temp[period,i] <- readSingleResult(SWMMfile, iType=1, iIndex=iIndexes[i],
                                           vIndex=v, period=period-1) 
      }
    }
    results[[vars[v+1]]] <- xts(temp, SWMMfile$SWMMTimes)
  }

  results

}


##' Read link results from a SWMM file.
##'
##' The following variables are available for links:
##' \itemize{
##' \item{"flow rate"}{}
##' \item{"flow depth"}{}
##' \item{"flow velocity"}{}
##' \item{"Froude number"}{}
##' \item{"capacity"}{fraction of conduit filled}
##' \item{"PollutantName concentration"} {Only if pollutions are modeled.}
##' }
##' @title read SWM results for links
##' @param SWMMfile a \code{SWMMfile} object
##' @param names vector of names of the links to be read. Partial matching is supported.
##' @param variables vector of variables to be read for each link, see Details below. If \code{NULL}, all variables are read. Partial matching is supported.
##' @return A named list of separate \code{xts} objects for each variable.
##' The \code{xts} objects contain the data for all links.
##' @author Andreas Scheidegger
##' @export
readLinks <- function(SWMMfile, names, variables=NULL){

  ## -- get index of elements
  iIndexes <- pmatch(names, SWMMfile$linkNames) - 1
  if(any(is.na(iIndexes))){
    stop(paste0("The following link names(s) could not be found in the file:\n-  ",
                paste(names[is.na(iIndexes)], collapse="\n-  ")))
  }

  ## -- get variable index
  vars <- c("flow rate", "flow depth", "flow velocity", "Froude number", "capacity")
  if(length(SWMMfile$pollNames)>0){
    vars <- c(vars, paste(unlist(SWMMfile$pollNames), "concentration"))
  }
  
  if(is.null(variables)){
    vIndexes = 0:(length(vars)-1)
  } else {
    vIndexes <- pmatch(variables, vars) - 1
  }

  if(any(is.na(vIndexes))){
    stop(paste0("The following variable(s) could not be uniquely matched:\n-  ",
                paste(variables[is.na(vIndexes)], collapse="\n-  ")))
  }
  
  ## read 
  results <- list()
  for(v in vIndexes){
    temp <- matrix(NA, ncol=length(iIndexes), nrow=SWMMfile$numReportingPeriods)
    colnames(temp) <- names 
    for(i in seq_along(iIndexes)){
      for(period in 1:SWMMfile$numReportingPeriods){
        temp[period,i] <- readSingleResult(SWMMfile, iType=2, iIndex=iIndexes[i],
                                           vIndex=v, period=period-1) 
      }
    }
    results[[vars[v+1]]] <- xts(temp, SWMMfile$SWMMTimes)
  }

  results 
}



##' Reads the system results from a SWMM file.
##'
##' The following variables are available for the system:
##' \itemize{
##' \item{"air temperature"}{}
##' \item{"rainfall"}{}
##' \item{"snow depth"}{}
##' \item{"evaporation + infiltration loss rate"}{}
##' \item{"runoff flow"}{}
##' \item{"dry weather inflow"}{}
##' \item{"groundwater inflow"}{}
##' \item{"RDII inflow"}{}
##' \item{"direct inflow"}{user supplied}
##' \item{"total lateral inflow"}{sum of runoff flow plus all inflows}
##' \item{"flow lost to flooding"}{}
##' \item{"flow leaving through outfalls"}{}
##' \item{"volume of stored water"}{}
##' \item{"actual evaporation rate"}{}
##' \item{"potential evaporation rate"}
##' }
##' @title read SWM results for the system
##' @param SWMMfile a \code{SWMMfile} object
##' @param variables vector of variables to be read, see Details below. If \code{NULL}, all variables are read. Partial matching is supported.
##' @return A named list of \code{xts} objects for each variable.
##' @author Andreas Scheidegger
##' @export
readSystem <- function(SWMMfile, variables=NULL){

  ## -- get variable index
  vars <- c("air temperature", "rainfall", "snow depth", "evaporation + infiltration loss rate",
            "runoff flow", "dry weather inflow", "groundwater inflow", "RDII inflow",
            "direct inflow", "total lateral inflow", "flow lost to flooding",
            "flow leaving through outfalls", "volume of stored water",
            "actual evaporation rate", "potential evaporation rate")
  
  if(is.null(variables)){
    vIndexes = 0:(length(vars)-1)
  } else {
    vIndexes <- pmatch(variables, vars) - 1
  }

  if(any(is.na(vIndexes))){
    stop(paste0("The following variable(s) could not be uniquely matched:\n-  ",
                paste(variables[is.na(vIndexes)], collapse="\n-  ")))
  }
  
  ## -- read
  results <- list()
  for(v in vIndexes){
    temp <- rep(NA, SWMMfile$numReportingPeriods) 
    for(period in 1:SWMMfile$numReportingPeriods){
      temp[period] <- readSingleResult(SWMMfile, iType=3, iIndex=0,
                                       vIndex=v, period=period-1) 
    }
    results[[vars[v+1]]] <- xts(temp, SWMMfile$SWMMTimes)
  }

  results 
}

