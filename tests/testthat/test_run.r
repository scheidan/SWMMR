## -------------------------------------------------------
## Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------

library(SWMMR)


context("Test building SWMM input files")

inf <-    "../testdata/input_template.inp"

test_that("Check if input files are correct", {

  infnew <- tempfile()
  expect_silent(buildInputFile(infnew, templatefile=inf, parameters=c(area1=111.111, imp1=22.22)))
  expect_error(buildInputFile(infnew, templatefile=inf, parameters=c(area1=111.111, imp1=22.22, notused=123)))
  expect_error(buildInputFile(infnew, templatefile=inf, parameters=c(area1=111.111)))
  expect_error(buildInputFile(infnew, templatefile=inf, parameters=c(area1=111.111, wrongname=22.22)))
})
