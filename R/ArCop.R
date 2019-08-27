#' Construction of copula object
#'
#' Constructs an ArCop object, either by utilising Type for known archimedean copulas or generator for an arbitrary generator function.
#'
#' @param Type character. Type can be supplied in order to construct a named copula. e.g. a Frank copula with type = "Frank".
#' @param param numeric. Supplies value of parameter(s) of the copula to be constructed.
#' @param dim interger. Supplies the number of dimensions of the copula to be constructed.
#' @param generator function. Supplies the
#'
#' @examples
#' \donttest{
#' ExCop <- copu()
#' ExCop
#' }
#'
#' @export

copu <- function(Type = "Unknown", param = NA, dim = 2L, generator = NULL)
{
  cdf = 0
  pdf = 0

  result <- list(dimension = dim,
                 parameters = param,
                 Name = Type,
                 distribution = c(cdf = cdf, pdf = pdf),
                 genfun = generator)

  class(result) <- 'ArCop'

  return(result)
}
