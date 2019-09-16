#' Distribution function of a copula
#'
#' Constructs the distribution function for a supplied copula object.
#'
#' @param copula ArCop object. Supplies the copula for which the cdf is to be defined.
#' @return An expression for the cdf of the supplied copula object.
#'
#' @examples
#' \donttest{
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exS <- rCop(copula = exC, n = 1000)
#' }
#'
#' @export

pCop <- function (copula) {
  d <- copula$dimension
  if (copula$Name == "Clayton"){
  theta <- copula$parameters
  expr <- "u1^(-theta) - 1"
  for (i in 2:d) {
    expr2 <- paste("u", i, "^(-theta) - 1", sep = "")
    expr <- paste(expr, expr2, sep=" + ")
  }
    expr <- paste("(1 + (", expr, "))^(-1/theta)")
    parse(text = expr)
  } else {
    stop("Please supply an ArCop object with a supported family specification")
  }
}
