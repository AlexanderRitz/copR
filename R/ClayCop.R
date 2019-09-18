#' Construction of copula object for the Clayton family
#'
#' Constructs a ClayCop object, holding all relevant information on a given
#' copula of the "Clayton" family. A parameter theta and the number of
#' dimension has to be supplied. Theta can only take positive values.
#' If the supplied parameter is too close to zero, a warning will be given;
#' the use of an Independence copula is recommended in this case.
#'
#' @param par numeric. Supplies value of parameter of the copula to be constructed.
#' @param dim interger. Supplies the number of dimensions of the copula to be constructed.
#' @return A list of class "ClayCop" with elements
#' \item{dimension}{Number of dimensions}
#' \item{expression}{List containing expression for generator function and its inverse}
#' \item{parameter}{Parameter value of generator function}
#' \item{prange}{vector of upper and lower bounds of the parameter}
#' \item{family}{Name of constructed copula, e.g. "Clayton"}
#' \item{distribution}{A list consisting of cdf and pdf}
#'
#' @examples
#' \donttest{
#' exCop <- ClayCop(par = 5, dim = 2)
#' summary(exCop)
#' }
#'
#' @export


ClayCop <- function (par = NA,
  dim = 2L) {
  if (par < 0) {
    stop("Parameter of Clayton family can not take negative values!")
  } else if (abs(par) <= .Machine$double.eps) {
    stop("Parameter is close enough to zero for a change to the Independence copula")
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
      expressions = list(gen = gen, invg = invg),
      parameter = par,
      prange = range,
      family = fam,
      distribution = list(cdf = cdf, pdf = pdf)
    )
    class(result) <- 'ClayCop'
    cdf <- pCop(result, eva = FALSE)
    result$distribution$cdf <- cdf
    pdf <- dCop(result, eva = FALSE)
    result$distribution$pdf <- pdf

    return(result)
  }
}
