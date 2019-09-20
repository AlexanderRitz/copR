context("Construction of cdf")

test_that("Clayton cdf is constructed correctly", {
  cc1 <- claycop(par = 5, dim = 2)
  expect_equal(as.character(cc1$distribution$cdf),
    "(1 + (u1^(-theta) - 1 + u2^(-theta) - 1))^(-1/theta)")
})

test_that("Frank cdf is constructed correctly", {
  fc1 <- frankcop(par = 5, dim = 2)
  expect_equal(as.character(fc1$distribution$cdf),
    "-1/theta * log(1 + exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta) - 1))")
})

test_that("Independence cdf is constructed correctly", {
  ic1 <- indcop(dim = 2)
  expect_equal(as.character(ic1$distribution$cdf),
    "u1 * u2")
})

test_that("Clayton cdf adheres to necessary properties", {
  cc2 <- claycop(par = 5, dim = 2)
  cdf <- pcop(cc2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(round(cdf, 4), 0.4366)

  cc3 <- claycop(par = 5, dim = 2)
  cdf <- pcop(cc3, eva = TRUE, u = c(0.5, 0))
  expect_equal(cdf, 0)

  cc4 <- claycop(par = 5, dim = 2)
  cdf <- pcop(cc4, eva = TRUE, u = c(0, 0.5))
  expect_equal(cdf, 0)

  cc5 <- claycop(par = 5, dim = 2)
  cdf <- pcop(cc5, eva = TRUE, u = c(0.5, 1))
  expect_equal(cdf, 0.5)

  cc6 <- claycop(par = 5, dim = 2)
  cdf <- pcop(cc6, eva = TRUE, u = c(1, 0.5))
  expect_equal(cdf, 0.5)

  cc7 <- claycop(par = 5, dim = 2)
  cdf <- pcop(cc7, eva = TRUE, u = c(1, 1))
  expect_equal(cdf, 1)
})

test_that("Frank cdf adheres to necessary properties", {
  fc2 <- frankcop(par = 5, dim = 2)
  cdf <- pcop(fc2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(round(cdf, 4), 0.3771)

  fc3 <- frankcop(par = 5, dim = 2)
  cdf <- pcop(fc3, eva = TRUE, u = c(0.5, 0))
  expect_equal(cdf, 0)

  fc4 <- frankcop(par = 5, dim = 2)
  cdf <- pcop(fc4, eva = TRUE, u = c(0, 0.5))
  expect_equal(cdf, 0)

  fc5 <- frankcop(par = 5, dim = 2)
  cdf <- pcop(fc5, eva = TRUE, u = c(0.5, 1))
  expect_equal(cdf, 0.5)

  fc6 <- frankcop(par = 5, dim = 2)
  cdf <- pcop(fc6, eva = TRUE, u = c(1, 0.5))
  expect_equal(cdf, 0.5)

  fc7 <- claycop(par = 5, dim = 2)
  cdf <- pcop(fc7, eva = TRUE, u = c(1, 1))
  expect_equal(cdf, 1)
})

test_that("Independence cdf adheres to necessary properties", {
  ic2 <- indcop(dim = 2)
  cdf <- pcop(ic2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(round(cdf, 2), 0.25)

  ic3 <- indcop(dim = 2)
  cdf <- pcop(ic3, eva = TRUE, u = c(0.5, 0))
  expect_equal(cdf, 0)

  ic4 <- indcop(dim = 2)
  cdf <- pcop(ic4, eva = TRUE, u = c(0, 0.5))
  expect_equal(cdf, 0)

  ic5 <- indcop(dim = 2)
  cdf <- pcop(ic5, eva = TRUE, u = c(0.5, 1))
  expect_equal(cdf, 0.5)

  ic6 <- indcop(dim = 2)
  cdf <- pcop(ic6, eva = TRUE, u = c(1, 0.5))
  expect_equal(cdf, 0.5)

  ic7 <- indcop(dim = 2)
  cdf <- pcop(ic7, eva = TRUE, u = c(1, 1))
  expect_equal(cdf, 1)
})
