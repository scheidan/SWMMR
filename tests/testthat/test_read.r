## -------------------------------------------------------
## Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------


library(SWMMR)


context("Reading of SWMM binary output files")

## calculate a cheap check sum
checksum <- function(x) sum(x*rep(1:7, length=length(x)))


ff <- openSWMMOutput("../testdata/output")

test_that("file properties are correct", {
  expect_equal(ff$numSubc, 1)
  expect_equal(ff$numNode, 1)
  expect_equal(ff$numLink, 0)
  expect_equal(ff$numPoll, 0)
})

test_that("subcatchments information is correct", {
  expect_equal(length(readSubcatchments(ff, names=c("S1"))), ff$numSubcVars)
  
  ## checksums.S1 <- c(rainfall=232.99, `snow depth`=0, `evaporation + infiltration losses`=0,
  ##                   runoff=223.4777, `groundwater outflow`=333.9936,
  ##                   `groundwater water table elevation`=0)
  ## for(var in names(checksums.S1)){
  ##   expect_equal(readSubcatchments(ff, names=c("S1"), variables=var), checksums.S1[var])
  ## }
})

test_that("node information is correct", {
  expect_equal(length(readNodes(ff, names=c("O1"))), ff$numNodeVar)
})


## test_that("link information is correct", {
##   expect_equal(length(readLinks(ff, names=c("L1"))), ff$numLinkPropSaved)
## })


test_that("system information is correct", {
  expect_equal(length(readSystem(ff)), ff$numSysVars)
})


closeSWMMOutput(ff)
