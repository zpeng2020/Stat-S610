context("Check local linear regression function")
source("llr_functions.R")

library(testthat)

n = 15
## a very simple regression model

x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
omega = 1

## Testing examples: 8 testings

test_that("llr output has correct length", {
    ## 1 testings
    ## check the output of llr is that it should be the same length as z mx1
    expect_equal(length(llr(x, y, z, omega = 1)), length(z))
  })


test_that("make_weight_matrix works on simple cases", {
  ## 4 testings
  ## using the first element of z=-1 and making the testings
  Wz = make_weight_matrix(z[1],x,omega)
  ## check that the output is a diagonal matrix, that elements off-diagonal are zeros, elements on-diagonal are non-negative
  expect_true(all(Wz[upper.tri(Wz,diag = FALSE)] ==0))
  expect_true(all(Wz[lower.tri(Wz,diag = FALSE)] ==0))
  expect_true(all(diag(Wz) >=0))
  ## check the output when z=-1
  r =  abs(x - z[1]) / omega
  expect_equal(diag(Wz),sapply(r, W))
})


test_that("make_predictor_matrix works on simple cases", {
    ## 3 testings
    ## write tests to check that the dimensions are correct
    X = make_predictor_matrix(x) 
    ## check the dimensions nx2
    expect_equal(dim(X), c(n,2))
    ## the first column is all 1's
    expect_true(all(X[,1] == 1))
    ## the second column is x
    expect_equal(X[,2], x)
}) 