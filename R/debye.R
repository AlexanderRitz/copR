#' Utility to calculate the parameter theta of a Frank copula for a given value
#' of Kendall's tau
#'
#' Simplistic implementation of the Debye function of order one. A
#' simplification of an implementation of Diethelm Wuertz in the package
#' "fCopulae".
#'
#' @param x double. A numeric value as input for the Debye function of order one.
#' @return The value of the Debye function of order one at x.


debye <- function(x) {
  fun <- function(x, lambda) {
    x ^ lambda / (exp(x) - 1)
  }
  up = abs(x)
  if (x == 0) {
    D = 1
  } else {
    int = stats::integrate(
      f = fun,
      lower = 0,
      upper  = up,
      lambda = 1
    )
    D = int[[1]] / up
  }
  if (x < 0) {
    D = D + up / (2)
  }

  return(D)
}
