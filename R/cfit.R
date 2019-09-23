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
#' @return A list of class "fitcop" inheriting the class of the supplied copula
#' object, with identical elements to the original object, aside from the
#' maximum likelihood estimate replacing the original parameter value. With one
#' additional element
#' \item{loglikelihood}{The maximum value of the log-likelihood of the supplied
#' copula.}
#'
#' @examples
#' \donttest{
#' exc <- claycop(par = 5, dim = 2)
#' U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
#' byrow = TRUE)
#' fit <- cfit(copula = exc, data = U)
#'
#' # Choice of an interval could be based on a visual inspection of the
#' #log-likelihood function:
#' xpar <- seq(from = 0.1, to = 10, length.out = 100)
#' ll <- rep(NA, times = 100)
#' for (i in 1:100) {
#'   ll[i] <- cloglik(exc, data = U, parameter = xpar[i])
#'   }
#' plot(x = xpar, y = ll, xlab = "Parameter value", ylab = "log-likelihood")
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
  objective <- result$objective
  cl <- class(optmodel)
  class(optmodel) <- c("fitcop", cl)
  optmodel$loglikelihood <- objective
  return(optmodel)
}
