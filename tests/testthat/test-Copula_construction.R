context("Copula object construction")

test_that("Clayton copulas with correct input are correctly created", {
  cc <- claycop(par = 5, dim = 3)
  expect_is(cc, "claycop")

  cc2 <- claycop(par = 5)
  expect_is(cc2, "claycop")
})

test_that("Frank copulas with correct input are correctly created", {
  fc <- frankcop(par = 5, dim = 3)
  expect_is(fc, "frankcop")

  fc2 <- frankcop(par = -5, dim = 2)
  expect_is(fc2, "frankcop")

  fc3 <- frankcop(par = -5)
  expect_is(fc3, "frankcop")
})

test_that("Independence copulas with correct input are correctly created", {
  ic <- indcop(dim = 3)
  expect_is(ic, "indcop")

  ic2 <- indcop(dim = 2)
  expect_is(ic2, "indcop")
})

test_that("invalid input for dim results in error", {
  expect_error(cc3 <- claycop(par = 5, dim = 1), "at least 2 dimensions")

  expect_error(fc4 <- frankcop(par = 5, dim = 1), "at least 2 dimensions")

  expect_error(ic3 <- indcop(dim = 1), "at least 2 dimensions")

  expect_error(cc4 <- claycop(par = 5, dim = c(2, 3)), "integer")

  expect_error(fc5 <- frankcop(par = 5, dim = c(2, 3)), "integer")

  expect_error(ic4 <- indcop(dim = c(2, 3)), "integer")
})

test_that("invalid input for par results in error", {
  expect_error(cc5 <- claycop(par = -2, dim = 3), "negative")

  expect_error(cc6 <- claycop(dim = 3), "supplied")

  expect_error(fc6 <- frankcop(par = -3, dim = 3), "dimensions")

  expect_error(fc7 <- frankcop(dim = 3), "supplied")

  expect_error(cc7 <- claycop(par = c(2, 3), dim = 3), "single")

  expect_error(fc8 <- frankcop(par = c(2, 3), dim = 3), "single")

  expect_error(cc8 <- claycop(par = .Machine$double.eps, dim = 3),
    "Independence")

  expect_error(fc9 <- frankcop(par = .Machine$double.eps, dim = 3),
    "Independence")
})
