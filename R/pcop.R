#' Distribution function of a copula
#'
#' Constructs the distribution function for a supplied copula object.
#'
#' @param copula A copula object. Supplies the copula for which the cdf is to be
#' defined.
#' @param u double. Optional argument supplying the data to evaluate the cdf.
#' @return An expression for the cdf of the supplied copula object or its
#' numeric value at the supplied u in case evaluation was chosen..
#'
#' @examples
#' \donttest{
#' excop <- Claycop(par = 5, dim = 2)
#' excdf <- pcop(excop)
#' #In case evaluation of the cdf is wanted:
#' excdfval <- pcop(excop, u = c(0.5, 0.5))
#' }
#'
#' @export

pcop <- function(copula, u){
  UseMethod("pcop")
}
