context("Fitting of copula parameters with maximum likelihood")

test_that("Clayton copula is fitted correctly", {
  cc1 <- claycop(par = 5, dim = 2)
  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  fit1 <- cfit(copula = cc1, data = U)
  par <- fit1$parameter
  ll <- fit1$loglikelihood
  expect_equal(round(par, digits = 3), 2.026)

  expect_equal(round(ll, digits = 4), 0.4417)

  fit2 <- cfit(copula = cc1, data = U, interval = c(1, 3))
  par2 <- fit2$parameter
  ll2 <- fit2$loglikelihood
  expect_equal(round(par2, digits = 3), 2.026)

  expect_equal(round(ll2, digits = 4), 0.4417)

})

test_that("Frank copula is fitted correctly", {
  fc1 <- frankcop(par = 5, dim = 2)
  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  fit3 <- cfit(copula = fc1, data = U)
  par <- fit3$parameter
  ll <- fit3$loglikelihood
  expect_equal(round(par, digits = 3), 2.964)

  expect_equal(round(ll, digits = 5), 0.05561)

  fit4 <- cfit(copula = fc1, data = U, interval = c(1, 3))
  par2 <- fit4$parameter
  ll2 <- fit4$loglikelihood
  expect_equal(round(par2, digits = 3), 2.964)

  expect_equal(round(ll2, digits = 5), 0.05561)
})

test_that("Invalid input is handled correctly", {
  cc2 <- claycop(par = 5, dim = 2)
  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  expect_error(fit5 <- cfit(copula = cc2, data = U, interval = c(1, 2, 3)),
    "appropriate length")

  expect_error(fit6 <- cfit(copula = cc2, data = U, interval = c(1, 1)),
    "distinct")

  expect_error(fit7 <- cfit(copula = cc2, data = U, interval = c(NA, "hey")),
    "numeric")

  fc2 <- frankcop(par = 5, dim = 2)
  expect_error(fit10 <- cfit(copula = fc2, data = U, interval = c(1, 2, 3)),
    "appropriate length")

  expect_error(fit11 <- cfit(copula = fc2, data = U, interval = c(1, 1)),
    "distinct")

  expect_error(fit12 <- cfit(copula = fc2, data = U, interval = c(NA, "hey")),
    "numeric")

  ic1 <- indcop(dim = 2)
  expect_error(fit8 <- cfit(copula = ic1, data = U),
    "does not rely on any parameter")

  x <- 7
  expect_error(fit9 <- cfit(copula = x, data = U),
    "not supported")
})

test_that("Model selection criteria give correct values", {
  cc3 <- claycop(par = 5, dim = 2)
  U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
    byrow = TRUE)
  fit13 <- cfit(copula = cc3, data = U)
  AIC <- c_aic(fit13)
  expect_equal(round(AIC, 5), 1.11667)

  BIC <- c_bic(fit13)
  expect_equal(round(BIC, 5), 0.21529)

  fc3 <- frankcop(par = 5, dim = 2)
  fit14 <- cfit(copula = fc3, data = U)
  AIC2 <- c_aic(fit14)
  expect_equal(round(AIC2, 5), 1.88879)

  BIC2 <- c_bic(fit14)
  expect_equal(round(BIC2, 5), 0.9874)

})
