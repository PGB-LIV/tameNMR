
library(testthat)
source("lib.R")
# tests for functions in GalaxyTools/lib.R

script.dir <- dirname(dirname(sys.frame(1)$ofile))

test_that("Table dimensions are correct",{
  data = readNMRTabFile(paste(script.dir, '/test_data/data.csv',sep=''))
  writeNMRTabFile(data, paste(script.dir, '/test_data/testWrite.csv', sep=''))
  data2 = readNMRTabFile(paste(script.dir, '/test_data/testWrite.csv', sep=''))
  expect_that(dim(data), equals(dim(data2)))
  })


test_that("ParsePPMintervals works correctly",{
  dat = matrix(c(0,6,9,12,5,8,11,14), 4, 2)
  expect_that(dat, equals(parsePpmIntervals('0-5,12-14,8-6,11-9')))
  # TODO: add overlap merge
  })


