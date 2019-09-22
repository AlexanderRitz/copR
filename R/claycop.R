#' Construction of copula object for the Clayton family
#'
#' Constructs a claycop object, holding all relevant information on a given
#' copula of the "Clayton" family. A parameter called theta and the number of
#' dimensions has to be supplied. In this implementation Theta can only take
#' positive values. If the supplied parameter is too close to zero, you will be
#' informed that the use of an Independence copula is recommended in this case.
#'
#' @param par numeric. Supplies value of parameter of the copula to be
#' constructed.
#' @param dim integer. Supplies the number of dimensions of the copula to be
#' constructed.
#' @return A list of class "claycop" with elements
#' \item{dimension}{Number of dimensions}
#' \item{generator}{List containing expressions for the generator function and
#' its inverse}
#' \item{parameter}{Parameter value of generator function}
#' \item{prange}{Vector of upper and lower bounds of permitted parameter values}
#' \item{family}{Name of constructed copula family, e.g. "Clayton"}
#' \item{distribution}{A list consisting of expressions for cdf and pdf}
#' The expressions can be evaluated for the parameter theta and additionally the
#' variable "t" in case of the generator function, "s" for its inverse and
#' variables "ui" in the case of cdf and pdf.
#'
#' @examples
#' \donttest{
#' excop <- claycop(par = 5, dim = 2)
#' summary(excop)
#' }
#'
#' @export


claycop <- function (par = NA,
  dim = 2L) {
   if (length(par) != 1L) {
    stop(
      "The Clayton family only relies on a single parameter."
      )
  } else if ((is.na(par) || is.null(par))) {
    stop(
      "Parameter value has to be supplied."
    )
  } else if (length(dim) != 1) {
    stop(
      "Dimension not of correct format, please supply a single integer."
    )
  } else if (par < 0) {
    stop(
      "Parameter of Clayton family can not take negative values!"
      )
  } else if (abs(par) <= (.Machine$double.eps ^ (1 / 2))) {
    stop(
      "Parameter is close enough to zero to change to the Independence copula."
      )
  } else if (dim < 2) {
    stop(
      "A copula can only be constructed for at least 2 dimensions!"
      )
  } else {
    fam <- "Clayton"
    gen <- expression(t ^ (-theta) - 1)
    invg <- expression((1 + s) ^ (-(1 / theta)))
    lowb <- 0L
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
    class(result) <- c("claycop", "copula")
    cdf <- pcop(result, eva = FALSE)
    result$distribution$cdf <- cdf
    pdf <- dcop(result, eva = FALSE)
    result$distribution$pdf <- pdf

    return(result)
  }
}
