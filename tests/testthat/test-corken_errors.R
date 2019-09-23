context("Kendall's tau incorrect input is handled correctly")

test_that("Invalid arguments result in error", {
  U1 <- matrix(c(0.7026, 0.6359, NA, 0.8833, 0.3127, 0.4035), ncol = 3,
    byrow = TRUE)
  expect_error(corken(U1, fast = TRUE), "missing")

  U2 <- matrix(c("Robocop", "is", NA, "here", "for", "puns"), ncol = 3,
    byrow = TRUE)
  expect_error(corken(U2), "numeric")

  U3 <- c(1, 2, 3)
  expect_error(corken(U3), "matrix or")
})
