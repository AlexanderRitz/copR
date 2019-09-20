context("Construction of cdf")

test_that("Clayton cdf is constructed correctly", {
  cc1 <- clayCop(par = 5, dim = 2)
  expect_equal(as.character(cc1$distribution$cdf),
    "(1 + (u1^(-theta) - 1 + u2^(-theta) - 1))^(-1/theta)")
})

test_that("Frank cdf is constructed correctly", {
  fc1 <- frankCop(par = 5, dim = 2)
  expect_equal(as.character(fc1$distribution$cdf),
    "-1/theta * log(1 + exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta) - 1))")
})

test_that("Clayton cdf adheres to necessary properties", {
  cc2 <- clayCop(par = 5, dim = 2)
  cdf <- pCop(cc2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(round(cdf, 4), 0.4366)

  cc3 <- clayCop(par = 5, dim = 2)
  cdf <- pCop(cc3, eva = TRUE, u = c(0.5, 0))
  expect_equal(cdf, 0)

  cc4 <- clayCop(par = 5, dim = 2)
  cdf <- pCop(cc4, eva = TRUE, u = c(0, 0.5))
  expect_equal(cdf, 0)

  cc5 <- clayCop(par = 5, dim = 2)
  cdf <- pCop(cc5, eva = TRUE, u = c(0.5, 1))
  expect_equal(cdf, 0.5)

  cc6 <- clayCop(par = 5, dim = 2)
  cdf <- pCop(cc6, eva = TRUE, u = c(1, 0.5))
  expect_equal(cdf, 0.5)

  cc7 <- clayCop(par = 5, dim = 2)
  cdf <- pCop(cc7, eva = TRUE, u = c(1, 1))
  expect_equal(cdf, 1)
})

test_that("Frank cdf adheres to necessary properties", {
  fc2 <- frankCop(par = 5, dim = 2)
  cdf <- pCop(fc2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(round(cdf, 4), 0.3771)

  fc3 <- frankCop(par = 5, dim = 2)
  cdf <- pCop(fc3, eva = TRUE, u = c(0.5, 0))
  expect_equal(cdf, 0)

  fc4 <- frankCop(par = 5, dim = 2)
  cdf <- pCop(fc4, eva = TRUE, u = c(0, 0.5))
  expect_equal(cdf, 0)

  fc5 <- frankCop(par = 5, dim = 2)
  cdf <- pCop(fc5, eva = TRUE, u = c(0.5, 1))
  expect_equal(cdf, 0.5)

  fc6 <- frankCop(par = 5, dim = 2)
  cdf <- pCop(fc6, eva = TRUE, u = c(1, 0.5))
  expect_equal(cdf, 0.5)

  fc7 <- clayCop(par = 5, dim = 2)
  cdf <- pCop(fc7, eva = TRUE, u = c(1, 1))
  expect_equal(cdf, 1)
})
