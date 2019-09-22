#' Utility to construct matrix of function values for plotting functions
#'
#' Calculates a matrix containing the function values for a supplied function
#' and two input variables.
#'
#' @param x double. First input variable.
#' @param y double. Second input variable.
#' @param f expression. Gives the function to be evaluated.
#' @param par double.
#' @return A matrix containing the function values of f for the inputs x and y.

app <- function (x, y, f, par) {
  v <- matrix(NA, nrow = length(x), ncol = length(y))
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      theta <- par
      u1 <- x[i]
      u2 <- y[j]
      v[i, j] <- eval(f)
    }
  }
  return(v)
}
