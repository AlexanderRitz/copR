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
#' @param ... Arguments to be passed to corken, regarding treatment of missing
#' values or faster function choice.
#' @return A copula object with parameter theta chosen by inversion of Kendall's
#' tau.
#'
#' @references
#' Frees and Valdez (1998). Understanding Relationships Using Copulas. North
#' American Actuarial Journal 2(1):1-25. January 1998
#' @references Hofert et al. (2018). Elements of Copula Modeling with R.
#' Springer.
#' @references
#' Nelsen (2006). An introduction to copulas. Springer Series in Statistics.
#' Second Edition.
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

cmethmo <- function (copula,
  data = NULL,
  tau = NULL,
  ...) {
  if (copula$dimension != 2) {
    stop("Only a bivariate copula may be fitted with this method.")
  } else {
    if (is.null(tau)) {
      if (ncol(data) == 2 && !is.null(data)) {
        tau <- corken(data = data, ...)[1, 2]
        if (is.na(tau)) {
          stop("Missing values have to be addressed in order to calculate tau.")
        }
        if (is.claycop(copula)) {
          if (tau == 1) {
            theta <- Inf
          } else if (tau == -1) {
            theta <- -Inf
          } else {
            theta <- (-2 * tau / (tau - 1))
          }
          newcop <- claycop(par = theta, dim = 2)
        } else if (is.frankcop(copula)) {
          if (tau == 0) {
            theta <- 0
            newcop <- frankcop(par = theta, dim = 2)
          } else {
            theta <- franktau(tau)
            newcop <- frankcop(par = theta, dim = 2)
          }
        } else if (is.indcop(copula)) {
          stop("No parameter to be chosen in case of independence!")
        } else {
          stop("Supplied copula object is not supported currrently.")
        }
        return(newcop)
      } else {
        stop("Data of dimension (n x 2) has to be supplied!")
      }
    } else if (length(tau) == 1) {
      if (tau <= 1 && tau >= -1) {
        if (is.claycop(copula)) {
          if (tau == 1) {
            theta <- Inf
          } else if (tau == -1) {
            theta <- -Inf
          } else {
            theta <- (-2 * tau / (tau - 1))
          }
          newcop <- claycop(par = theta, dim = 2)
        } else if (is.frankcop(copula)) {
          if (tau == 0) {
            theta <- 0
            newcop <- frankcop(par = theta, dim = 2)
          } else {
            theta <- franktau(tau)
            newcop <- frankcop(par = theta, dim = 2)
          }
        } else if (is.indcop(copula)) {
          stop("No parameter to be chosen in case of independence!")
        } else {
          stop("Supplied copula object is not supported currrently.")
        }
        return(newcop)
      } else {
        stop("Tau has to take a value in [-1, 1].")
      }
    }
  }
}
