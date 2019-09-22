#' Fitting of copula parameter by maximum likelihood
#'
#' Calculates the maximum of the log-Likelihood function of a chosen Copula
#' family by utilizing stats::optimise.
#'
#' @param copula A copula object. Decides the copula family for which the
#' log-Likelihood function is to be maximised.
#' @param data Matrix or dataframe of appropriate dimension (n x d).
#' The data to base the Likelihood on. Data points have to be normed, i.e.
#' copula data has to lie within [0, 1]^d.
#' @param interval double. Optional argument, a vector to supply a lower and
#' upper bound of the interval to be searched for the maximum. If not supplied,
#' a very extensive interval within the appropriate parameter space will be
#' chosen. Plotting the log-Likelihood function to decide upon an interval is
#' recommended.
#' @return A list containing a copula object with parameter theta chosen by
#' maximum Likelihood estimation and the result of the optimisation of the
#' log-Likelihood function.
#'
#' @examples
#' \donttest{
#' exc <- claycop(par = 5, dim = 2)
#' U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833), nrow = 2, byrow = TRUE)
#' ll <- cloglik(copula = exc, data = U)
#' }
#'
#' @export


cfit <- function (copula, data, interval = NULL) {
  if (is.null(interval)) {
    if (is.claycop(copula)) {
      lowb <- copula$prange[1]
      upb <- 300
    } else if (is.frankcop(copula)) {
      if (copula$dimension == 2) {
        lowb <- -80
        upb <- 80
      } else {
        lowb <- 0
        upb <- 300
      }
    } else if (is.indcop(copula)) {
      stop("The Independence copula does not rely on any parameter.")
    } else {
      stop("Supplied copula object is not supported.")
    }
  } else if (length(interval) == 2) {
    if (interval[1] < interval[2]) {
      lowb <- interval[1]
      upb <- interval[2]
    } else if (interval[2] < interval[1]) {
      lowb <- interval[2]
      upb <- interval[1]
    } else {
      stop("Please supply two distinct numeric interval bounds.")
    }
  } else {
    stop(
      "Interval not of appropriate length. Should contain a lower and upper
      bound for the parameter space to be searched."
    )
  }
  result <- stats::optimise(
    f = cloglik,
    interval = c(lowb, upb),
    maximum = TRUE,
    copula = copula,
    data = data
  )
  if (copula$family == "Clayton") {
    optmodel <- claycop(par = as.numeric(result[1]), dim = copula$dimension)
  } else if (copula$family == "Frank") {
    optmodel <- frankcop(par = as.numeric(result[1]), dim = copula$dimension)
  }
  opmosum <- list(optmodel, result)
  return(opmosum)
}
