#' Construction of copula object for the Frank family
#'
#' Constructs a frankCop object, holding all relevant information on a given
#' copula of the "Frank" family. A parameter called theta and the number of
#' dimensions has to be supplied. In this implementation Theta can only take
#' positive values. If the supplied parameter is too close to zero, you will be
#' informed that the use of an Independence copula is recommended in this case.
#'
#' @param par numeric. Supplies value of parameter of the copula to be
#' constructed.
#' @param dim integer. Supplies the number of dimensions of the copula to be
#' constructed.
#' @return A list of class "frankCop" with elements
#' \item{dimension}{Number of dimensions}
#' \item{generator}{List containing expressions for the generator function and
#' its inverse}
#' \item{parameter}{Parameter value of generator function}
#' \item{prange}{Vector of upper and lower bounds of the parameter}
#' \item{family}{Name of constructed copula, e.g. "Frank"}
#' \item{distribution}{A list consisting of cdf and pdf}
#'#' The expressions can be evaluated for the parameter theta and additionally the
#' variable "t" in case of the generator function, "s" for its inverse and
#' variables "ui" in the case of cdf and pdf.
#'
#' @examples
#' \donttest{
#' exCop <- frankCop(par = 5, dim = 2)
#' summary(exCop)
#' }
#'
#' @export

frankCop <- function (par = NA,
  dim = 2L) {
  if (length(par) != 1L) {
    stop(
      "The Frank family only relies on a single parameter."
      )
  } else if (length(dim) != 1) {
    stop(
      "Dimension not of correct format, please supply a single integer."
    )
  } else if (par < 0 && dim > 2) {
    stop(
      "Parameter of Frank family can not take negative values for dimensions
      higher than 2!"
      )
  } else if (abs(par) <= (.Machine$double.eps ^ (1 / 2))) {
    stop(
      "Parameter is close enough to zero to change to the Independence copula"
      )
  } else if (dim < 2) {
    stop(
      "A copula can only be constructed for at least 2 dimensions!"
      )
  } else {
    fam <- "Frank"
    gen <-
      expression(-log(t * (exp(-theta * t) - 1) / (exp(-theta) - 1)))
    invg <-
      expression(-(1 / theta) * log(1 - exp(-s) * (1 - exp(-theta))))
    lowb <- -Inf
    upb <- Inf
    range <- c(lowb, upb)

    cdf <- NULL
    pdf <- NULL
    result <- list(
      dimension = dim,
      generator = list(gen = gen, invg = invg),
      parameter = par,
      prange = range,
      family = fam,
      distribution = list(cdf = cdf, pdf = pdf)
    )
    class(result) <- 'frankCop'
    cdf <- pCop(result, eva = FALSE)
    result$distribution$cdf <- cdf
    pdf <- dCop(result, eva = FALSE)
    result$distribution$pdf <- pdf

    return(result)
  }
}
