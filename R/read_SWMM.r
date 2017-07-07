## -------------------------------------------------------
## Andreas Scheidegger -- andreas.scheidegger@eawag.ch
## -------------------------------------------------------

##' opens a binay SWMM output file
##'
##' @title Open a SWMM output file
##' @param SWMMoutfile character, path to output file
##' @return a SWMM file handler
##' @author Andreas Scheidegger
##' @export
openSWMMOutput <- function(SWMMoutfile) {
  
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
  if(f.props$SWMMversion < 51000){
    stop("At least SWMM V5.1 is required!")
  }
  
  f.props$numSubc <- header[4]
  f.props$numNode <- header[5]
  f.props$numLink <- header[6]
  f.props$numPoll <- header[7]
  f.props$unitCode <- header[3]
  
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
      } else if(unitCode==1) {
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
  
  f.props$numSysVars <- readBin(f,integer(), n=1,size=4)
  f.props$sysVarCodes <- readBin(f,integer(), n=f.props$numSysVars,size=4)

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
    as.POSIXct(strptime("12/30/1899", format="%m/%d/%Y", tz="GMT"))

  
  f.props$outFileHandle <- f 
  return(f.props)
}



##' @title Closes a SWMM file handler
##' @param SWMMfile a SWMM file handler
##' @return nothing
##' @author Andreas Scheidegger
##' @export
closeSWMMOutput <- function(SWMMfile){
  close(SWMMfile$outFileHandle)
} 



##' @title Extract properties of a SWMM output file
##' @param SWMMfile a SWMM file handler
##' @return list containing the most important properties
##' @author Andreas Scheidegger
##' @export
properties <- function(SWMMfile){
  ll <- list()

  ll$subcatchmentNames <- unlist(SWMMfile$subcNames)
  ll$linkNames <- unlist(SWMMfile$linkNames)
  ll$nodes <- data.frame(name=unlist(SWMMfile$nodeNames),
                         type=unlist(SWMMfile$nodeType)) 

  if(SWMMfile$numPoll > 0) {
    ll$pollutants <- data.frame(name=unlist(SWMMfile$pollNames),
                                unit=unlist(SWMMfile$pollUnits))
  }

  ll$deltaTime <- diff(SWMMfile$SWMMTimes[1:2])
  ll$minTime <- min(SWMMfile$SWMMTimes)
  ll$maxTime <- max(SWMMfile$SWMMTimes)

  return(ll)
}


## reading per variable is very slow as a lot of jumping is required... maybe read all data per time step instead?

readSingleResult <- function(SWMMfile, iType, iIndex, vIndex, period){
  ## Reads a *single* SWMM result. 
  ## See the documentation in SWMM intefacing for detail
  ## iType is either 0,1,2,3 for subcatch, node, link or sys variable
  ## vIndex is the index of the variable for subcatch, node, link or sys object
  ## iIndex is the position of the subcatch, node, or link among the other subcatch, links, nodes
  ## I would recommend using getSWMMTimeSeries so that you don't have to know iIndex.  That function is a wrapper
  ## around this one and looks up results based on names not indicies.

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



readSubcatchments <- function(SWMMfile, names, variables="all") {
  ## Number of subcatchment variables (currently 6 + number of pollutants).
  ## Code number of each subcatchment variable:
  ## 0 for rainfall (in/hr or mm/hr),
  ## 1 for snow depth (in or mm),
  ## 2 for evaporation + infiltration losses (in/hr or mm/hr),
  ## 3 for runoff rate (flow units),
  ## 4 for groundwater outflow rate (flow units),
  ## 5 for groundwater water table elevation (ft or m),
  ## 6 for runoff concentration of first pollutant,
  ## 5 + N for runoff concentration of N-th pollutant.

  iType <- 0

  ## get index of elements
  iIndexes <- which(SWMMfile$subcNames %in% name) - 1

  ## get variable index
  vars <- c("rainfall", "snow depth", "evaporation + infiltration losses", "runoff",
            "groundwater outflow", "groundwater water table elevation", unlist(SWMMfile$pollNames))
  if(variables=="all") {
    vIndexes = 0:(length(vars)-1)
  } else {
    vIndexes <- pmatch(variables, vars) - 1
  }

  for(i in iIndexes){
    for(v in vIndexes){
      output <- array(NA, SWMMfile$numReportingPeriods)
      for(period in 1:SWMMfile$numReportingPeriods){
        output[period] <- readSingleResult(SWMMfile, iType=iType, iIndex=i,
                                           vIndex=c, period=period-1) 
      }
    }
  }

  output

}



getSWMMTimeSeriesData <- function(SWMMfile, name, iType, vIndex){
  ## SWMMfile should be an object obtained by calling openSWMMOut
  ## iType should be 0 for Subcatchments
  ##                1 for nodes
  ##                2 for links
  ##                3 for system variables
  ## name should be the exact name in the output file of a subcatchment,
  ## link, or node, or leave this as an empty string if searching for system results
  ## vIndex should be selected from this lists below,depending on whether iType is a subcatchment,
  ## link or node.
  ## #
  ## # BEGIN vIndex choices###############################
  ## #
  ## Number of subcatchment variables (currently 6 + number of pollutants).
  ## Code number of each subcatchment variable:
  ## 0 for rainfall (in/hr or mm/hr),
  ## 1 for snow depth (in or mm),
  ## 2 for evaporation + infiltration losses (in/hr or mm/hr),
  ## 3 for runoff rate (flow units),
  ## 4 for groundwater outflow rate (flow units),
  ## 5 for groundwater water table elevation (ft or m),
  ## 6 for runoff concentration of first pollutant,
  ## 5 + N for runoff concentration of N-th pollutant.
  ##
  ## Number of node variables (currently 6 + number of pollutants)
  ## Code number of each node variable:
  ## 0 for depth of water above invert (ft or m),
  ## 1 for hydraulic head (ft or m),
  ## 2 for volume of stored + ponded water (ft3 or m3),
  ## 3 for lateral inflow (flow units),
  ## 4 for total inflow (lateral + upstream) (flow units),
  ## 5 for flow lost to flooding (flow units),
  ## 6 for concentration of first pollutant,
  ## 5 + N for concentration of N-th pollutant.
  ##
  ## Number of link variables (currently 5 + number of pollutants)
  ## Code number of each link variable:
  ##
  ## 0 for flow rate (flow units),
  ## 1 for flow depth (ft or m),
  ## 2 for flow velocity (ft/s or m/s),
  ## 3 for Froude number,
  ## 4 for capacity (fraction of conduit filled),
  ## 5 for concentration of first pollutant,
  ## 4 + N for concentration of N-th pollutant.
  ##
  ## Number of system-wide variables (currently 14)
  ## Code number of each system-wide variable:
  ## 0 for air temperature (deg. F or deg. C),
  ## 1 for rainfall (in/hr or mm/hr),
  ## 2 for snow depth (in or mm),
  ## 3 for evaporation + infiltration loss rate (in/hr or mm/hr),
  ## 4 for runoff flow (flow units),
  ## 5 for dry weather inflow (flow units),
  ## 6 for groundwater inflow (flow units),
  ## 7 for RDII inflow (flow units),
  ## 8 for user supplied direct inflow (flow units),
  ## 9 for total lateral inflow (sum of variables 4 to 8) (flow units),
  ## 10 for flow lost to flooding (flow units),
  ## 11 for flow leaving through outfalls (flow units),
  ## 12 for volume of stored water (ft3 or m3),
  ## 13 for evaporation rate (in/day or mm/day)
  ##

  if(iType==0){
    iIndex <- (0:(SWMMfile$numSubc-1))[SWMMfile$subcNames==name]
  } else if(iType==1){
    iIndex <- (0:(SWMMfile$numNode-1))[SWMMfile$nodeNames==name]
  } else if(iType==2){
    iIndex <- (0:(SWMMfile$numLink-1))[SWMMfile$linkNames==name]
  } else if(iType==3){
    iIndex <- 0
  }
  output <- array(NA, SWMMfile$numReportingPeriods)
  for(period in 0:(-1+SWMMfile$numReportingPeriods)){
    output[period+1] <- readSingleResult(SWMMfile=SWMMfile, iType=iType, iIndex=iIndex,
                                         vIndex=vIndex, period=period)
  }
  return(output)
}

