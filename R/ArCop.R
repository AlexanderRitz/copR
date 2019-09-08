#' Construction of copula object
#'
#' Constructs an ArCop object, either by utilising Type for known archimedean copulas or generator for an arbitrary generator function.
#'
#' @param Type character. Type can be supplied in order to construct a named copula. e.g. a Frank copula with type = "Frank".
#' @param par numeric. Supplies value of parameter(s) of the copula to be constructed.
#' @param dim interger. Supplies the number of dimensions of the copula to be constructed.
#' @return A list of class "ArCop" with elements
#' \item{dimension}{Number of dimensions}
#' \item{expression}{List containing expression for generator function and its inverse}
#' \item{parameters}{Parameter value(s) of generator function}
#' \item{prange}{vector of upper and lower bound(s) of the parameter(s)}
#' \item{Name}{Name of constructed copula, e.g. "Clayton"}
#' \item{distribution}{list consisting of cdf and pdf}
#'
#' @examples
#' \donttest{
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exC
#' }
#'
#' @export

copu <- function(Type = "Unknown",
  par = NA,
  dim = 2L) {
  if (Type == "Clayton") {
    gen <- expression(t ^ (-theta) - 1)
    invg <- expression((1 + s) ^ (-(1 / theta)))
    lowb <- 0L
    upb <- Inf
    range <- c(lowb, upb)

  } else {
    stop(
      "Given family of copula not implemented. Please check availabe families by looking up the allowed Types."
    )
  }
  cdf = 0
  pdf = 0
  result <- list(
    dimension = dim,
    expressions = list(gen = gen, invg = invg),
    parameters = par,
    prange = range,
    Name = Type,
    distribution = list(cdf = cdf, pdf = pdf)
  )
  class(result) <- 'ArCop'
  return(result)
}
