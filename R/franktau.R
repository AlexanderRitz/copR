#' Utility to calculate the parameter theta of a Frank copula for a given value
#' of Kendall's tau
#'
#' Implementation of the relationship between theta and tau for a Frank copula.
#' A simplification of an implementation of Diethelm Wuertz in the package
#' "fCopulae". Original implementation falsely gives value of "Inf" for
#' tau = -1, this has been fixed in this implementation.
#'
#' @param tau double. A numeric value for Kendall's tau.
#' @return A value for the parameter theta based on method of moments estimation
#' by the supplied Kendalls'tau.


franktau <- function(tau) {
  sign <- 1
  if (tau < 0) {
    sign <- -1
    tau <- -tau
  }
  if (abs(tau) > 0.99999) {
    return(Inf * sign)
  }
  thet <- function(x) {
    tau - (1 - 4 / x + 4 / x * debye(x))
  }
  rooted <- stats::uniroot(
    thet,
    lower = 0 + .Machine$double.eps ^ (1 / 2),
    upper = 5e+05,
    tol = .Machine$double.eps ^ (1 / 2)
  )
  theta <- rooted$root
  return(sign * theta)
}
