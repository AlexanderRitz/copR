context("Copula object construction")

test_that("Clayton copulas with correct input are correctly created", {
  cc <- clayCop(par = 5, dim = 3)
  expect_is(cc, "clayCop")
})

test_that("Frank copulas with correct input are correctly created", {
  fc <- frankCop(par = 5, dim = 3)
  expect_is(fc, "frankCop")

  fc2 <- frankCop(par = -5, dim = 2)
  expect_is(fc2, "frankCop")
})

test_that("invalid input for dim results in error", {
  expect_error(cc2 <- clayCop(par = 5, dim = 1), "at least 2 dimensions")

  expect_error(fc3 <- frankCop(par = 5, dim = 1), "at least 2 dimensions")

  expect_error(cc3 <- clayCop(par = 5, dim = c(2, 3)), "integer")

  expect_error(fc4 <- frankCop(par = 5, dim = c(2, 3)), "integer")
})

test_that("invalid input for par results in error", {
  expect_error(cc4 <- clayCop(par = -2, dim = 3), "negative")

  expect_error(fc5 <- frankCop(par = -3, dim = 3), "dimensions")

  expect_error(cc5 <- clayCop(par = c(2, 3), dim = 3), "single")

  expect_error(fc6 <- frankCop(par = c(2, 3), dim = 3), "single")

  expect_error(cc6 <- clayCop(par = .Machine$double.eps, dim = 3),
    "Independence")

  expect_error(fc7 <- frankCop(par = .Machine$double.eps, dim = 3),
    "Independence")
})
