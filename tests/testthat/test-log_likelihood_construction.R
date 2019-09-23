context("Log-likelihood value is correctly calculated")

test_that("Clayton log-likelihood is calculated correctly", {
  cc1 <- claycop(par = 5, dim = 2)
  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  ll1 <- cloglik(cc1, data = U, parameter = 0)
  expect_equal(ll1, 0)

  ll2 <- cloglik(cc1, data = U, parameter = 0.5)
  expect_equal(round(ll2, digits = 4), 0.1943)

  ll3 <- cloglik(cc1, data = U, parameter = 1)
  expect_equal(round(ll3, digits = 4), 0.3319)
})

test_that("Frank log-likelihood is calculated correctly", {
  fc1 <- frankcop(par = 5, dim = 2)
  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  ll4 <- cloglik(fc1, data = U, parameter = 0)
  expect_equal(ll4, 0)

  ll5 <- cloglik(fc1, data = U, parameter = 0.5)
  expect_equal(round(ll5, digits = 4), 0.0118)

  ll6 <- cloglik(fc1, data = U, parameter = 1)
  expect_equal(round(ll6, digits = 4), 0.0239)
})

test_that("Invalid input is handled correctly", {
  cc2 <- claycop(par = 5, dim = 2)
  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 2,
    byrow = TRUE)
  expect_error(ll7 <- cloglik(copula = cc2, data = U, parameter = 2),
    "Dimension")

  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  expect_error(ll8 <- cloglik(copula = cc2, data = U, parameter = c(2, 1)),
    "single parameter")

  expect_error(ll9 <- cloglik(copula = cc2, data = U, parameter = -1),
    "negative")

  fc2 <- frankcop(par = 5, dim = 3)
  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  expect_error(ll10 <- cloglik(copula = fc2, data = U, parameter = 2),
    "Dimension")

  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 2,
    byrow = TRUE)
  expect_error(ll11 <- cloglik(copula = fc2, data = U, parameter = c(2, 1)),
    "single parameter")

  expect_error(ll12 <- cloglik(copula = fc2, data = U, parameter = -1),
    "negative")
})
