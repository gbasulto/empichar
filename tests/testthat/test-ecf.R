context("test-ecf")

test_that("multivariate sample at multiple vectors", {
  nn <- 500
  smp <- cbind(rnorm(nn), rnorm(nn))
  x <- matrix(c(0, 0, 0, 1), ncol = 2)
  out <- ecf(x, smp)
  expect_true(length(out) == 2)
})


test_that("multivariate sample at single vector", {
  nn <- 500
  smp <- cbind(rnorm(nn), rnorm(nn))
  out <- ecf(0:1, smp)
  expect_true(length(out) == 1)
})

test_that("univariate sample at multiple points", {
  out <- ecf(seq(-2, 2, length.out = 100), rnorm(10))
  expect_true(length(out) == 100)
})


test_that("univariate sample at single point", {
  out <- ecf(0, rnorm(10))
  expect_true(length(out) == 1)
})

test_that("complex number", {
  out <- ecf(0, rnorm(10))
  expect_true(is.complex(out))
})
