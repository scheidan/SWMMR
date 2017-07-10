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

3. Install SWMMR (type in the R command line)
```
library(devtools)
install_github("scheidan/SWMMR")
```


## Usage

This is a minimal example:
```R
library(SWMMR)
...
```

## Acknowledgments
This package is based on [this](https://github.com/PeterDSteinberg/RSWMM) work of Peter Steinberg.