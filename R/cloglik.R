#' Computation of loglikelihood of Copulas
#'
#' Calculates the loglikelihood function of a chosen Copula family
#'
#' @param copula A copula object. Decides the copula family for which the
#' Likelhood function is to be calculated.
#' @param data The data to base the Likelihood on.
#' @param parameter Parameter value of the copula.
#' @return The value of the calculated loglikelihood function.
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
  if (is.null(parameter)) {
    robocop <- copula
  } else {
    if (length(parameter) != 1) {
      stop(
        "Only a single parameter value should be supplied."
        )
    } else if (parameter < 0) {
      if (is.frankcop(copula) && copula$dimension == 2) {
        robocop <- frankcop(par = parameter, dim = copula$dimension)
      } else {
        stop(
        "Parameter value can only take negative values in case of bivariate
        Frank copula."
          )
      }
    } else {
      if (is.claycop(copula)) {
        robocop <- claycop(par = parameter, dim = copula$dimension)
      } else if (is.frankcop(copula)) {
        robocop <- frankcop(par = parameter, dim = copula$dimension)
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
