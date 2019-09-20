context("Copula sampling")

test_that("Clayton samples are of correct dimensions", {
  cc1 <- claycop(par = 5, dim = 3)
  ccs1 <- rcop(cc1, n = 100)
  expect_equal(dim(ccs1), c(100L, 3L))
})

test_that("Frank samples are of correct dimensions", {
  fc1 <- frankcop(par = 5, dim = 3)
  fcs1 <- rcop(fc1, n = 100)
  expect_equal(dim(fcs1), c(100L, 3L))

  fc2 <- frankcop(par = 5, dim = 2)
  fcs2 <- rcop(fc2, n = 100)
  expect_equal(dim(fcs2), c(100L, 2L))

  fc3 <- frankcop(par = -5, dim = 2)
  fcs3 <- rcop(fc3, n = 100)
  expect_equal(dim(fcs3), c(100L, 2L))
})

test_that("Independence samples are of correct dimensions", {
  ic1 <- indcop(dim = 3)
  ics1 <- rcop(ic1, n = 100)
  expect_equal(dim(ics1), c(100L, 3L))
})

test_that("Clayton samples do not contain NA, NULL or NaN", {
  cc2 <- claycop(par = 5, dim = 3)
  ccs2 <- rcop(cc2, n = 100)
  expect_equal(anyNA(ccs2), FALSE)

  cc3 <- claycop(par = 5, dim = 3)
  ccs3 <- rcop(cc3, n = 100)
  expect_equal(any(is.nan((ccs3))), FALSE)
})

test_that("Frank samples do not contain NA or NaN", {
  fc4 <- frankcop(par = 5, dim = 3)
  fcs4 <- rcop(fc4, n = 100)
  expect_equal(anyNA(fcs4), FALSE)

  fc5 <- frankcop(par = 5, dim = 2)
  fcs5 <- rcop(fc5, n = 100)
  expect_equal(anyNA(fcs5), FALSE)

  fc6 <- frankcop(par = -5, dim = 2)
  fcs6 <- rcop(fc6, n = 100)
  expect_equal(anyNA(fcs6), FALSE)

  fc7 <- frankcop(par = 5, dim = 3)
  fcs7 <- rcop(fc7, n = 100)
  expect_equal(any(is.nan((fcs7))), FALSE)

  fc8 <- frankcop(par = 5, dim = 2)
  fcs8 <- rcop(fc8, n = 100)
  expect_equal(any(is.nan((fcs8))), FALSE)

  fc9 <- frankcop(par = -5, dim = 2)
  fcs9 <- rcop(fc9, n = 100)
  expect_equal(any(is.nan((fcs9))), FALSE)
})

test_that("Independence samples do not contain NA, NULL or NaN", {
  ic2 <- indcop(dim = 3)
  ics2 <- rcop(ic2, n = 100)
  expect_equal(anyNA(ics2), FALSE)

  ic3 <- indcop(dim = 3)
  ics3 <- rcop(ic3, n = 100)
  expect_equal(any(is.nan((ics3))), FALSE)
})
