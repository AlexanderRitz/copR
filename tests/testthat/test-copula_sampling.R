context("Copula sampling")

test_that("Clayton samples are of correct dimensions", {
  cc1 <- clayCop(par = 5, dim = 3)
  ccs1 <- rCop(cc1, n = 100)
  expect_equal(dim(ccs1), c(100L, 3L))
})

test_that("Frank samples are of correct dimensions", {
  fc1 <- frankCop(par = 5, dim = 3)
  fcs1 <- rCop(fc1, n = 100)
  expect_equal(dim(fcs1), c(100L, 3L))

  fc2 <- frankCop(par = 5, dim = 2)
  fcs2 <- rCop(fc2, n = 100)
  expect_equal(dim(fcs2), c(100L, 2L))

  fc3 <- frankCop(par = -5, dim = 2)
  fcs3 <- rCop(fc3, n = 100)
  expect_equal(dim(fcs3), c(100L, 2L))
})

test_that("Independence samples are of correct dimensions", {
  ic1 <- indCop(dim = 3)
  ics1 <- rCop(ic1, n = 100)
  expect_equal(dim(ics1), c(100L, 3L))
})

test_that("Clayton samples do not contain NA, NULL or NaN", {
  cc2 <- clayCop(par = 5, dim = 3)
  ccs2 <- rCop(cc2, n = 100)
  expect_equal(anyNA(ccs2), FALSE)

  cc3 <- clayCop(par = 5, dim = 3)
  ccs3 <- rCop(cc3, n = 100)
  expect_equal(any(is.nan((ccs3))), FALSE)
})

test_that("Frank samples do not contain NA or NaN", {
  fc4 <- frankCop(par = 5, dim = 3)
  fcs4 <- rCop(fc4, n = 100)
  expect_equal(anyNA(fcs4), FALSE)

  fc5 <- frankCop(par = 5, dim = 2)
  fcs5 <- rCop(fc5, n = 100)
  expect_equal(anyNA(fcs5), FALSE)

  fc6 <- frankCop(par = -5, dim = 2)
  fcs6 <- rCop(fc6, n = 100)
  expect_equal(anyNA(fcs6), FALSE)

  fc7 <- frankCop(par = 5, dim = 3)
  fcs7 <- rCop(fc7, n = 100)
  expect_equal(any(is.nan((fcs7))), FALSE)

  fc8 <- frankCop(par = 5, dim = 2)
  fcs8 <- rCop(fc8, n = 100)
  expect_equal(any(is.nan((fcs8))), FALSE)

  fc9 <- frankCop(par = -5, dim = 2)
  fcs9 <- rCop(fc9, n = 100)
  expect_equal(any(is.nan((fcs9))), FALSE)
})

test_that("Independence samples do not contain NA, NULL or NaN", {
  ic2 <- indCop(dim = 3)
  ics2 <- rCop(ic2, n = 100)
  expect_equal(anyNA(ics2), FALSE)

  ic3 <- indCop(dim = 3)
  ics3 <- rCop(ic3, n = 100)
  expect_equal(any(is.nan((ics3))), FALSE)
})
