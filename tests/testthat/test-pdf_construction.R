context("Construction of pdf")

test_that("Clayton pdf is constructed correctly", {
  cc1 <- claycop(par = 5, dim = 2)
  expect_equal(as.character(cc1$distribution$pdf),
    "(1 + (u1^(-theta) - 1 + u2^(-theta) - 1))^(((-1/theta) - 1) - 1) * (((-1/theta) - 1) * (u2^((-theta) - 1) * (-theta))) * ((-1/theta) * (u1^((-theta) - 1) * (-theta)))")
})

test_that("Frank pdf is constructed correctly", {
  fc1 <- frankcop(par = 5, dim = 2)
  expect_equal(as.character(fc1$distribution$pdf),
    "-1/theta * (exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta * u2) * theta/(exp(-theta) - 1)/((exp(-theta * u2) - 1)/(exp(-theta) - 1))) * (exp(-theta * u1) * theta/(exp(-theta) - 1)/((exp(-theta * u1) - 1)/(exp(-theta) - 1))) * (exp(-theta) - 1)/(1 + exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta) - 1)) - exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + \n    -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta * u1) * theta/(exp(-theta) - 1)/((exp(-theta * u1) - 1)/(exp(-theta) - 1))) * (exp(-theta) - 1) * (exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta * u2) * theta/(exp(-theta) - 1)/((exp(-theta * u2) - 1)/(exp(-theta) - 1))) * (exp(-theta) - 1))/(1 + exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta) - \n    1))^2)")
})

test_that("Independence pdf is constructed correctly", {
  ic1 <- indcop(dim = 2)
  expect_equal(as.character(ic1$distribution$pdf),
    "1")
})

test_that("Clayton pdf adheres to necessary properties", {
  cc2 <- claycop(par = 5, dim = 2)
  pdf <- dcop(cc2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(round(pdf, 4), 2.7037)

  cc3 <- claycop(par = 5, dim = 2)
  pdf <- dcop(cc3, eva = TRUE, u = c(0.5, 0))
  expect_equal(pdf, 0)

  cc4 <- claycop(par = 5, dim = 2)
  pdf <- dcop(cc4, eva = TRUE, u = c(0, 0.5))
  expect_equal(pdf, 0)

  cc5 <- claycop(par = 5, dim = 2)
  pdf <- dcop(cc5, eva = TRUE, u = c(0.5, 1))
  expect_equal(pdf, 0)

  cc6 <- claycop(par = 5, dim = 2)
  pdf <- dcop(cc6, eva = TRUE, u = c(1, 0.5))
  expect_equal(pdf, 0)

  cc7 <- claycop(par = 5, dim = 2)
  pdf <- dcop(cc7, eva = TRUE, u = c(-1, 0.5))
  expect_equal(pdf, 0)

  cc8 <- claycop(par = 5, dim = 2)
  pdf <- dcop(cc8, eva = TRUE, u = c(1.1, 0.5))
  expect_equal(pdf, 0)
})

test_that("Frank pdf adheres to necessary properties", {
  fc2 <- frankcop(par = 5, dim = 2)
  pdf <- dcop(fc2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(round(pdf, 4), 1.4736)

  fc3 <- frankcop(par = 5, dim = 2)
  pdf <- dcop(fc3, eva = TRUE, u = c(0.5, 0))
  expect_equal(pdf, 0)

  fc4 <- frankcop(par = 5, dim = 2)
  pdf <- dcop(fc4, eva = TRUE, u = c(0, 0.5))
  expect_equal(pdf, 0)

  fc5 <- frankcop(par = 5, dim = 2)
  pdf <- dcop(fc5, eva = TRUE, u = c(0.5, 1))
  expect_equal(pdf, 0)

  fc6 <- frankcop(par = 5, dim = 2)
  pdf <- dcop(fc6, eva = TRUE, u = c(1, 0.5))
  expect_equal(pdf, 0)

  fc7 <- claycop(par = 5, dim = 2)
  pdf <- dcop(fc7, eva = TRUE, u = c(-1, 0.5))
  expect_equal(pdf, 0)

  fc8 <- claycop(par = 5, dim = 2)
  pdf <- dcop(fc8, eva = TRUE, u = c(1.1, 0.5))
  expect_equal(pdf, 0)
})


test_that("Independence pdf adheres to necessary properties", {
  ic2 <- indcop(dim = 2)
  pdf <- dcop(ic2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(pdf, 1)

  ic3 <- indcop(dim = 2)
  pdf <- dcop(ic3, eva = TRUE, u = c(0.5, 0))
  expect_equal(pdf, 0)

  ic4 <- indcop(dim = 2)
  pdf <- dcop(ic4, eva = TRUE, u = c(0, 0.5))
  expect_equal(pdf, 0)

  ic5 <- indcop(dim = 2)
  pdf <- dcop(ic5, eva = TRUE, u = c(0.5, 1))
  expect_equal(pdf, 0)

  ic6 <- indcop(dim = 2)
  pdf <- dcop(ic6, eva = TRUE, u = c(1, 0.5))
  expect_equal(pdf, 0)

  ic7 <- indcop(dim = 2)
  pdf <- dcop(ic7, eva = TRUE, u = c(-1, 0.5))
  expect_equal(pdf, 0)

  ic8 <- indcop(dim = 2)
  pdf <- dcop(ic8, eva = TRUE, u = c(1.1, 0.5))
  expect_equal(pdf, 0)
})
