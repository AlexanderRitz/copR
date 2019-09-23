context("Fitting of copula parameters with method of moments")

test_that("Clayton copula is fitted correctly", {
  cc1 <- claycop(par = 5, dim = 2)
  fit1 <- cmethmo(cc1, tau = 0.5)
  par <- fit1$parameter
  expect_equal(par, 2)

  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  fit2 <- cmethmo(cc1, data = U)
  par2 <- fit2$parameter
  par2
  expect_equal(par2, 1)
})

test_that("Frank copula is fitted correctly", {
  fc1 <- frankcop(par = 5, dim = 2)
  fit2 <- cmethmo(fc1, tau = 0.5)
  par <- fit2$parameter
  expect_equal(round(par, digits = 4), 5.7363)

  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  fit3 <- cmethmo(fc1, data = U)
  par2 <- fit3$parameter
  expect_equal(round(par2, digits = 4), 3.3058)
})


test_that("Invalid input is handled correctly", {
  cc2 <- claycop(par = 5, dim = 2)
  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), ncol = 3,
    byrow = TRUE)
  expect_error(fit4 <- cmethmo(copula = cc2, data = U),
    "dimension")

  expect_error(fit5 <- cmethmo(copula = cc2, data = NULL),
    "has to be supplied")

  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  expect_error(fit6 <- cmethmo(copula = cc2, tau = -1.5),
    "has to take")

  expect_error(fit7 <- cmethmo(copula = cc2, tau = 1.5),
    "has to take")

  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), ncol = 3,
    byrow = TRUE)
  fc2 <- frankcop(par = 5, dim = 2)
  expect_error(fit8 <- cmethmo(copula = fc2, data = U),
    "dimension")

  expect_error(fit9 <- cmethmo(copula = fc2, data = NULL),
    "has to be supplied")

  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  expect_error(fit10 <- cmethmo(copula = fc2, tau = -1.5),
    "has to take")

  expect_error(fit11 <- cmethmo(copula = fc2, tau = 1.5),
    "has to take")

  U <- matrix(c(0.7026, NA, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  expect_error(fit12 <- cmethmo(copula = fc2, data = U),
    "Missing value")

  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  ic1 <- indcop(dim = 2)
  expect_error(fi13 <- cmethmo(copula = ic1, data = U),
    "No parameter")

  x <- 7
  expect_error(fit14 <- cmethmo(copula = x, data = U),
    "not supported")

})
