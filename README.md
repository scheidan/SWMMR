SWMMR - Modify inputs and read SWMM outputs from R
==================================================

_SWMMR_ is an R package to interface the EPA Storm Water Management
Model (SWMM). It can modify SWMM input files, run SWMM, and read SWMM
binary output files.


## Installation from Github

1. Install `devtools` (type in the R command line)
```
install.packages("devtools")
```

2. Install SWMMR (type in the R command line)
```
library(devtools)
install_github("scheidan/SWMMR")
```


## Usage

This is a minimal example, see the package help for more options.
```R
library(SWMMR)

## Run SWMM
runSWMM <- function(inputfile = "input.inp",
                    reportfilename="report.rpt",
                    outputfilename="output.out",
                    SWMMexe="swmm5.exe")

## Read output file
resfile <- openSWMMOutput("output.out")

resfile                                        # print summary

readSubcatchments(resfile, names=c("S1", "S2", "S3"), variables=c("rainfall", "runoff"))

readNodes(resfile, names="Nod1")               # read all variables for node "Nod1"

readLinks(resfile, names=NULL, variables=NULL) # read all variables for all links

readSystem(resfile)                            # read all system variables

closeSWMMOutput(resfile)                       # close file
```

## Alternatives
An alternative is the [_swmmr_](https://github.com/dleutnant/swmmr) package by Dominik Leutnant.
It should be faster (due file reader written in C++), the interface is more low-level compared to _SWMMR_.

## Acknowledgments
This package is based on [this](https://github.com/PeterDSteinberg/RSWMM) work of Peter Steinberg.
