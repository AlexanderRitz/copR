#' Fitting of bivariate copula parameter by method of moments,
#' using Kendall's tau
#'
#' Calculates a method of moments estimate of the parameter of a chosen
#' bivariate Copula.
#'
#' @param copula A copula object. Decides the copula family for which the
#' parameter is to be estimated.
#' @param data Matrix or dataframe of appropriate dimension (n x 2).
#' The data to base the dependence measure on. Optional if "tau" is supplied.
#' @param tau double. A value to take as Kendall's tau, to base the method of
#' moments estimator on.
#' @return A copula object with parameter theta chosen by inversion of Kendall's
#' tau.
#'
#' @examples
#' \donttest{
#' exc <- claycop(par = 5, dim = 2)
#' U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 3.2175, 4.0357), nrow = 3,
#' byrow = TRUE)
#' mmc <- cmethmo(copula = exc, data = U)
#' }
#'
#' @export

cmethmo <- function (copula, data = NULL, tau = NULL) {
  if (is.null(tau)) {
    if (ncol(data) == 2 && !is.null(data)) {
      tau <- corken(data = data)[1, 2]
      if (is.claycop(copula)) {
        theta <- (-2 * tau / (tau - 1))
      } else if (is.frankcop(copula)) {
        stop("No closed form solution exists, implementation outstanding.")
      } else if (is.indcop(copula)) {
        stop("No parameter to be chosen in case of independence!")
      } else {
        stop("Supplied copula object is not supported currrently.")
      }
      newcop <- claycop(par = theta, dim = 2)
      return(newcop)
    } else {
      stop("Data of dimension (n x 2) has to be supplied!")
    }
  } else if (length(tau) == 1) {
    if (tau <= 1 && tau >= -1) {
      if (is.claycop(copula)) {
        theta <- (-2 * tau / (tau - 1))
      } else if (is.frankcop(copula)) {
        stop("No closed form solution exists, implementation outstanding.")
      } else if (is.indcop(copula)) {
        stop("No parameter to be chosen in case of independence!")
      } else {
        stop("Supplied copula object is not supported currrently.")
      }
      newcop <- claycop(par = theta, dim = 2)
      return(newcop)
    } else {
      stop("Tau has to take a value in [-1, 1].")
    }
  }
}
