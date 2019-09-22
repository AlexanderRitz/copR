#' Fitting of copula parameter by maximum likelihood
#'
#' Calculates the maximum of the log-Likelihood function of a chosen Copula
#' family by utilizing stats::optimise.
#'
#' @param copula A copula object. Decides the copula family for which the
#' log-Likelihood function is to be maximised.
#' @param data double. The data to base the Likelihood on. Data points have to
#' be normed, i.e. copula data has to lie within [0, 1]^d.
#' @param interval double. Optional argument, a vector to supply a lower and
#' upper bound of the interval to be searched for the maximum.
#' @return A list containing a copula object with parameter theta chosen by
#' maximum Likelihood estimation and the value of the log-Likelihood function at
#' the found optimum.
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
  lowb <-
  upb <-
  stats::optimise(
    f = cloglik,
    interval = c(lowb, upb),
    maximum = TRUE,
    copula = copula,
    data = data
  )
}
