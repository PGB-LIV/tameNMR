# test for functions in BinLib


library(testthat)
source("binLib.R")
# tests for functions in GalaxyTools/lib.R

test_that("Uniform binning is correct.",{

  ppms = seq(0, 10, length=101)
  data = matrix(rep(1, 1000), 100, 10)
  # 0.5ppm window
  integrals1 = matrix(rep(rep(5, 20), 10), 20, 10)
  expect_that(integrals1, equals(uniformBinning(data, ppms, 0.5)))

  # 1ppm window
  integrals2 = matrix(rep(rep(10, 10), 10), 10, 10)
  expect_that(integrals2, equals(uniformBinning(data, ppms, 1)))

  # 1.2 ppm
  integrals3 = matrix(rep(rep(12, 8), 10), 8, 10)
  expect_that(integrals3, equals(uniformBinning(data, ppms, 1.2)))

  })