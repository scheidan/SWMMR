## -------------------------------------------------------
## Andreas Scheidegger -- andreas.scheidegger@eawag.ch
## -------------------------------------------------------


checkSWMMForErrors <- function(outFile) {
  ## Checks a SWMM output file for errors
  f <- file(outFile,"rb")
  on.exit(close(f))
  
  seek(f,-6*4,"end")
  output <- list()
  output$position.objectID <- readBin(f, integer(), n = 1, size = 4)
  output$position.objectProperties <- readBin(f, integer(), n = 1, size = 4)
  output$position.computedResults <- readBin(f, integer(), n = 1, size = 4)
  output$numReportingPeriods <- readBin(f, integer(), n = 1, size = 4)
  output$errorStatus <- readBin(f, integer(), n = 1, size = 4)
  return(output$errorStatus)
}


openSWMMOut <- function(outFile) {
  
  RECORDSIZE <- 4 
  f <- file(outFile, "rb")
  ## on.exit(close(f))

  output <- list()
  
  ## ---
  ## read the header of a binary output file

  header <- readBin(f, integer(), n = 7, size = 4)

  if(header[0] != 516114522){
    stop(paste0("'", outFile,"' is not a valid SWMM file!"))
  }

  output$SWMMversion <- output$header[2]
  output$numSubc <- output$header[4]
  output$numNode <- output$header[5]
  output$numLink <- output$header[6]
  output$numPoll <- output$header[7]
  output$unitCode <- output$header[3]
  
  seek(f,-6*4,"end")
  output$position.objectID <- readBin(f, integer(), n = 1, size = 4)
  output$position.objectProperties <- readBin(f, integer(), n = 1, size = 4)
  output$position.computedResults <- readBin(f, integer(), n = 1, size = 4)
  output$numReportingPeriods <- readBin(f, integer(), n = 1, size = 4)
  output$errorStatus <- readBin(f, integer(), n = 1, size = 4)
  
  if(output$errorStatus > 0){
    stop(paste("SWMM had error! Error code: ", output$errorStatus))
  }

  ## ---
  ## object names
  
  seek(f, output$position.objectID, "start");
  
  ## For all subcatchments
  output$subcNames <- list()
  if(output$numSubc > 0){
    for(i in 1:output$numSubc){
      lengthName <- readBin(f, integer(), n = 1, size = 4)
      output$subcNames[i] <- readChar(f, lengthName, useBytes = FALSE)
    }
  }
  
  ## For all nodes
  output$nodeNames <- list()
  if(output$numNode > 0){
    for(i in 1:output$numNode){
      lengthName <- readBin(f, integer(), n = 1, size = 4)
      output$nodeNames[i] <- readChar(f, lengthName, useBytes = FALSE)
    }
  }
  ## For all links
  output$linkNames <- list()
  if(output$numLink > 0) {
    for(i in 1:output$numLink){
      lengthName <- readBin(f, integer(), n = 1, size = 4)
      output$linkNames[i] <- readChar(f, lengthName, useBytes = FALSE)
    }
  }
  ## For all pollutants
  output$pollNames <- list()
  output$pollUnits <- list()
  if(output$numPoll > 0) {
    for(i in 1:output$numPoll){
      lengthName <- readBin(f, integer(), n = 1, size = 4)
      output$pollNames[i] <- readChar(f, lengthName, useBytes = FALSE)
    }
    for(i in 1:output$numPoll){
      unitCode <- readBin(f, integer(), n = 1, size = 4)
      if(unitCode==0){
        output$pollUnits[i] <- "mg/L"
      } else if(unitCode==1) {
        output$pollUnits[i] <- "ug/L"
      } else if(unitCode==2){
        output$pollUnits[i] <- "counts/L"
      }
    }
  }
  
  seek(f, output$position.objectProperties, "start")


  ## ---
  ## object properties
  
  ## Subcatchments
  output$numSubcPropSaved <- readBin(f, integer(), n = 1, size = 4)
  output$codesSubcPropSaved <- readBin(f, integer(), n = 1, size = 4)
  
  if (output$codesSubcPropSaved==1){
    output$subcArea <- readBin(f ,what="double", n=output$numSubc,size=4)
  }

  ## Nodes
  output$numNodePropSaved <- readBin(f, integer(), n = 1, size = 4)
  output$codesNodePropSaved <- readBin(f, integer(),n=output$numNodePropSaved,size=4)

  if(output$numNode>0){
    temp <- readBin(f,what="double",n=output$numNodePropSaved*output$numNode,size=4)
    codestemp <- temp[seq(from=1,by=3,to=length(temp))]
    output$nodeType <- list()
    count <- 0
    for(i in codestemp) {
      count <- count+1
      if(i==0){
        output$nodeType[count]="Junction"
      } else if(i==1){
        output$nodeType[count]="Outfall"
      } else if(i==2){
        output$nodeType[count]="Storage"
      } else if(i==3){
        output$nodeType[count]="Divider"
      }
    }
    output$nodeInvert <- temp[seq(from=2,by=3,to=length(temp))]
    output$nodeMaxDepth <- temp[seq(from=3,by=3,to=length(temp))]
  }


  ## Links
  output$numLinkPropSaved <- readBin(f, integer(), n = 1, size = 4)
  output$codesLinkPropSaved <- readBin(f, integer(), n =output$numLinkPropSaved, size = 4)

  if(output$numLink>0){
    temp <- readBin(f,what="double",n=output$numLinkPropSaved*output$numLink,size=4)
    codestemp <- temp[seq(from=1,by=5,to=length(temp))]
    output$linkType <- list()
    count <- 0
    for(i in codestemp){
      count <- count+1
      if(i==0){
        output$linkType[count]="Conduit"
      }else if(i==1){
        output$linkType[count]="Pump"
      }else if(i==2){
        output$linkType[count]="Orifice"
      }else if(i==3){
        output$linkType[count]="Weir"
      }else if(i==4){
        output$linkType[count]="Outlet"
      }
    }

    output$linkUpstreamInvertOffset <- temp[seq(from=2,by=5,to=length(temp))]
    output$linkDownstreamInvertOffset <- temp[seq(from=3,by=5,to=length(temp))]
    output$linkMaxDepth <- temp[seq(from=4,by=5,to=length(temp))]
    output$linkLength <- temp[seq(from=5,by=5,to=length(temp))]
    
  }

  ## ---
  ## other properties

  output$outFileHandle <- f
  
  output$numSubcVars <- readBin(f, integer(), n = 1, size = 4)

  output$subcVarCodes <- readBin(f,integer(),n=  output$numSubcVars,size=4)
  output$numNodeVars <- readBin(f, integer(), n = 1, size = 4)

  output$nodeVarCodes <- readBin(f,integer(), n=  output$numNodeVars,size=4)
  output$numLinkVars <- readBin(f, integer(), n = 1, size = 4)
  output$linkVarCodes <- readBin(f,integer(), n=  output$numLinkVars,size=4)
  output$numSysVars <- readBin(f,integer(),n=1,size=4)
  output$sysVarCodes <- readBin(f,integer(), n=output$numSysVars,size=4)

  output$bytesPerPeriod <- 2*RECORDSIZE +
    RECORDSIZE * (output$numSubc*(output$numSubcVars) +
                  output$numNode*(output$numNodeVars) +
                  output$numLink*(output$numLinkVars) +
                  output$numSysVars)

  return(output)
}


## add times to output list
getSWMMTimes <- function(headObj){
  ## gets the time stamps of the SWMM results in binary file
  f <- headObj$outFileHandle
  
  seek(f, headObj$position.computedResults, "start")

  headObj$SWMMTimes <- array(NA, headObj$numReportingPeriods)
  if(headObj$numReportingPeriods > 0){
    for(i in 1:headObj$numReportingPeriods){
      headObj$SWMMTimes[i] <- readBin(f,what="double",size=8,n=1)
      seek(f,headObj$bytesPerPeriod-8,"current")
    }
  } else {
    stop("No time steps listed in SWMM output file.")
  }
  
  ## Convert SWMM times to R POSIXlt datetimes
  headObj$SWMMTimes <- headObj$SWMMTimes*86400.0+as.POSIXct(strptime("12/30/1899", format="%m/%d/%Y",tz="GMT"))
  ## edit 2/10/2012 to force GMT time zone rather than locale specific
  return(headObj)
}


## HeadObj <- openSWMMout()
getSWMMResult <- function(headObj, iType, iIndex, vIndex, period){
  ## gets a single SWMM result.  See the documentation in SWMM intefacing for detail
  ## iType is either 0,1,2,3 for subcatch, node, link or sys variable
  ## vIndex is the index of the variable for subcatch, node, link or sys object
  ## iIndex is the position of the subcatch, node, or link among the other subcatch, links, nodes
  ## I would recommend using getSWMMTimeSeries so that you don't have to know iIndex.  That function is a wrapper
  ## around this one and looks up results based on names not indicies.

  ## 1/24/2012 edit: Fixed bug in returning results for models with pollutants
  RECORDSIZE <- 4
  SUBCATCH <- 0
  NODE <- 1
  LINK <- 2
  SYS <- 3

  f <- headObj$outFileHandle

  f <- file(outFile,"rb")
  on.exit(close(f))
  
  StartPos <- headObj$position.computedResults
  offset <- StartPos + period*(headObj$bytesPerPeriod) + 2*RECORDSIZE
  if (iType == SUBCATCH ){
    offset <- offset + RECORDSIZE*(iIndex*(headObj$numSubcVars) + vIndex)
  } else if (iType == NODE){
    offset <- offset + RECORDSIZE*(headObj$numSubc*(headObj$numSubcVars) +
                                   iIndex*(headObj$numNodeVars) + vIndex)
  } else if (iType == LINK){
    offset <- offset+ RECORDSIZE*(headObj$numSubc*(headObj$numSubcVars) +
                                  headObj$numNode*(headObj$numNodeVars) +
                                  iIndex*(headObj$numLinkVars) + vIndex)
  } else if (iType == SYS){
    offset <- offset + RECORDSIZE*(headObj$numSubc*(headObj$numSubcVars) +
                                   headObj$numNode*(headObj$numNodeVars) +
                                   headObj$numLink*(headObj$numLinkVars) + vIndex)
  }


  seek(f, offset, "start")
  output <- readBin(f, what="double" ,size=4, n=1)
  return(output)
}



getSWMMTimeSeriesData <- function(headObj, iType, nameInOutputFile, vIndex){
  ## headObj should be an object obtained by calling openSWMMOut
  ## iType should be 0 for Subcatchments
  ##                1 for nodes
  ##                2 for links
  ##                3 for system variables
  ## nameInOutputFile should be the exact name in the output file of a subcatchment,
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
    iIndex <- (0:(headObj$numSubc-1))[headObj$subcNames==nameInOutputFile]
  } else if(iType==1){
    iIndex <- (0:(headObj$numNode-1))[headObj$nodeNames==nameInOutputFile]
  } else if(iType==2){
    iIndex <- (0:(headObj$numLink-1))[headObj$linkNames==nameInOutputFile]
  } else if(iType==3){
    iIndex <- 0
  }
  output <- array(NA, headObj$numReportingPeriods)
  for(period in 0:(-1+headObj$numReportingPeriods)){
    output[period+1] <- getSWMMResult(headObj=headObj, iType=iType, iIndex=iIndex,
                                      vIndex=vIndex, period=period)
  }
  return(output)
}

