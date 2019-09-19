#' Construction of copula object for the Frank family
#'
#' Constructs a frankCop object.
#'
#' @param par numeric. Supplies value of parameter of the copula to be constructed.
#' @param dim interger. Supplies the number of dimensions of the copula to be constructed.
#' @return A list of class "frankCop" with elements
#' \item{dimension}{Number of dimensions}
#' \item{expression}{List containing expression for generator function and its inverse}
#' \item{parameter}{Parameter value of generator function}
#' \item{prange}{vector of upper and lower bounds of the parameter}
#' \item{family}{Name of constructed copula, e.g. "Frank"}
#' \item{distribution}{A list consisting of cdf and pdf}
#'
#' @examples
#' \donttest{
#' exCop <- frankCop(par = 5, dim = 2)
#' exCop
#' }
#'
#' @export

frankCop <- function (par = NA,
  dim = 2L) {
  fam <- "Frank"
  gen <- expression(-log(t * (exp(-theta * t) - 1) / (exp(-theta) - 1)))
  invg <- expression(-(1 / theta) * log(1 - exp(-s) * (1 - exp(-theta))))
  lowb <- -Inf
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
  class(result) <- 'frankCop'
  cdf <- pCop(result, eva = FALSE)
  result$distribution$cdf <- cdf
  pdf <- dCop(result, eva = FALSE)
  result$distribution$pdf <- pdf

  return(result)
}
