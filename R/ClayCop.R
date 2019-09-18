#' Construction of copula object for the Clayton family
#'
#' Constructs a ClayCop object.
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
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exC
#' }
#'
#' @export

ClayCop <- function (par = NA,
  dim = 2L) {
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
