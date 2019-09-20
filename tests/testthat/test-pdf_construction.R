context("Construction of pdf")

test_that("Clayton pdf is constructed correctly", {
  cc1 <- clayCop(par = 5, dim = 2)
  expect_equal(as.character(cc1$distribution$pdf),
    "(1 + (u1^(-theta) - 1 + u2^(-theta) - 1))^(((-1/theta) - 1) - 1) * (((-1/theta) - 1) * (u2^((-theta) - 1) * (-theta))) * ((-1/theta) * (u1^((-theta) - 1) * (-theta)))")
})

test_that("Frank pdf is constructed correctly", {
  fc1 <- frankCop(par = 5, dim = 2)
  expect_equal(as.character(fc1$distribution$pdf),
    "-1/theta * (exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta * u2) * theta/(exp(-theta) - 1)/((exp(-theta * u2) - 1)/(exp(-theta) - 1))) * (exp(-theta * u1) * theta/(exp(-theta) - 1)/((exp(-theta * u1) - 1)/(exp(-theta) - 1))) * (exp(-theta) - 1)/(1 + exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta) - 1)) - exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + \n    -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta * u1) * theta/(exp(-theta) - 1)/((exp(-theta * u1) - 1)/(exp(-theta) - 1))) * (exp(-theta) - 1) * (exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta * u2) * theta/(exp(-theta) - 1)/((exp(-theta * u2) - 1)/(exp(-theta) - 1))) * (exp(-theta) - 1))/(1 + exp(-(-log((exp(-theta * u1) - 1)/(exp(-theta) - 1)) + -log((exp(-theta * u2) - 1)/(exp(-theta) - 1)))) * (exp(-theta) - \n    1))^2)")
})

test_that("Independence pdf is constructed correctly", {
  ic1 <- indCop(dim = 2)
  expect_equal(as.character(ic1$distribution$pdf),
    "1")
})

test_that("Clayton pdf adheres to necessary properties", {
  cc2 <- clayCop(par = 5, dim = 2)
  pdf <- dCop(cc2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(round(pdf, 4), 2.7037)

  cc3 <- clayCop(par = 5, dim = 2)
  pdf <- dCop(cc3, eva = TRUE, u = c(0.5, 0))
  expect_equal(pdf, 0)

  cc4 <- clayCop(par = 5, dim = 2)
  pdf <- dCop(cc4, eva = TRUE, u = c(0, 0.5))
  expect_equal(pdf, 0)

  cc5 <- clayCop(par = 5, dim = 2)
  pdf <- dCop(cc5, eva = TRUE, u = c(0.5, 1))
  expect_equal(pdf, 0)

  cc6 <- clayCop(par = 5, dim = 2)
  pdf <- dCop(cc6, eva = TRUE, u = c(1, 0.5))
  expect_equal(pdf, 0)

  cc7 <- clayCop(par = 5, dim = 2)
  pdf <- dCop(cc7, eva = TRUE, u = c(-1, 0.5))
  expect_equal(pdf, 0)

  cc8 <- clayCop(par = 5, dim = 2)
  pdf <- dCop(cc8, eva = TRUE, u = c(1.1, 0.5))
  expect_equal(pdf, 0)
})

test_that("Frank pdf adheres to necessary properties", {
  fc2 <- frankCop(par = 5, dim = 2)
  pdf <- dCop(fc2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(round(pdf, 4), 1.4736)

  fc3 <- frankCop(par = 5, dim = 2)
  pdf <- dCop(fc3, eva = TRUE, u = c(0.5, 0))
  expect_equal(pdf, 0)

  fc4 <- frankCop(par = 5, dim = 2)
  pdf <- dCop(fc4, eva = TRUE, u = c(0, 0.5))
  expect_equal(pdf, 0)

  fc5 <- frankCop(par = 5, dim = 2)
  pdf <- dCop(fc5, eva = TRUE, u = c(0.5, 1))
  expect_equal(pdf, 0)

  fc6 <- frankCop(par = 5, dim = 2)
  pdf <- dCop(fc6, eva = TRUE, u = c(1, 0.5))
  expect_equal(pdf, 0)

  fc7 <- clayCop(par = 5, dim = 2)
  pdf <- dCop(fc7, eva = TRUE, u = c(-1, 0.5))
  expect_equal(pdf, 0)

  fc8 <- clayCop(par = 5, dim = 2)
  pdf <- dCop(fc8, eva = TRUE, u = c(1.1, 0.5))
  expect_equal(pdf, 0)
})


test_that("Independence pdf adheres to necessary properties", {
  ic2 <- indCop(dim = 2)
  pdf <- dCop(ic2, eva = TRUE, u = c(0.5, 0.5))
  expect_equal(pdf, 1)

  ic3 <- indCop(dim = 2)
  pdf <- dCop(ic3, eva = TRUE, u = c(0.5, 0))
  expect_equal(pdf, 0)

  ic4 <- indCop(dim = 2)
  pdf <- dCop(ic4, eva = TRUE, u = c(0, 0.5))
  expect_equal(pdf, 0)

  ic5 <- indCop(dim = 2)
  pdf <- dCop(ic5, eva = TRUE, u = c(0.5, 1))
  expect_equal(pdf, 0)

  ic6 <- indCop(dim = 2)
  pdf <- dCop(ic6, eva = TRUE, u = c(1, 0.5))
  expect_equal(pdf, 0)

  ic7 <- indCop(dim = 2)
  pdf <- dCop(ic7, eva = TRUE, u = c(-1, 0.5))
  expect_equal(pdf, 0)

  ic8 <- indCop(dim = 2)
  pdf <- dCop(ic8, eva = TRUE, u = c(1.1, 0.5))
  expect_equal(pdf, 0)
})
