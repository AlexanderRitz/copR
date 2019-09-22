#' Computation of log-Likelihood of Copulas
#'
#' Calculates the log-Likelihood function of a chosen Copula family.
#'
#' @param copula A copula object. Decides the copula family for which the
#' log-Likelihood function is to be calculated.
#' @param data The data to base the Likelihood on. Data points have to be normed.
#' Copula data has to lie within [0, 1]^d.
#' @param parameter double. Optional argument. Desired Parameter value in case
#' deviation from supplied copula object is desired.
#' @return The value of the calculated log-Likelihood function.
#'
#' @examples
#' \donttest{
#' exc <- claycop(par = 5, dim = 2)
#' U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833), nrow = 2, byrow = TRUE)
#' ll <- cloglik(copula = exc, data = U)
#' }
#'
#' @export

cloglik <- function(copula, data, parameter = NULL) {
  d <- copula$dimension
  n <- nrow(data)
  if (dim(data)[2] != d) {
    stop("Dimension of supplied copula and data are not identical!")
  }
  if (is.null(parameter)) {
    robocop <- copula
  } else {
    if (length(parameter) != 1) {
      stop(
        "Only a single parameter value should be supplied."
        )
    } else if (parameter < 0) {
      if (is.frankcop(copula) && d == 2) {
        robocop <- frankcop(par = parameter, dim = d)
      } else {
        stop(
        "Parameter value can only take negative values in case of bivariate
        Frank copula."
          )
      }
    } else {
      if (is.claycop(copula)) {
        robocop <- claycop(par = parameter, dim = d)
      } else if (is.frankcop(copula)) {
        robocop <- frankcop(par = parameter, dim = d)
      } else {
        stop(
          "Please supply a copula object of an appropriate Archimedean family."
          )
      }
    }
  }
  ll <- 0
  for (i in 1:n) {
    ll <- ll + log(dcop(robocop, u = data[i,]))
  }
  return(ll)
}
