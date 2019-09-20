#' Distribution function of a copula
#'
#' Constructs the distribution function for a supplied copula object.
#'
#' @param copula A copula object. Supplies the copula for which the cdf is to be
#' defined.
#' @param eva Logical. If eva = TRUE, the resulting expression for the cdf will
#' be evaluated for the supplied arguments.
#' @param u numeric. Optional argument supplying the data to evaluate the cdf,
#' only needed in case of eva = TRUE.
#' @return An expression for the cdf of the supplied copula object or its
#' numeric value at the supplied u in case evaluation was chosen..
#'
#' @examples
#' \donttest{
#' excop <- Claycop(par = 5, dim = 2)
#' excdf <- pcop(excop, eva = FALSE)
#' #In case evaluation of the cdf is wanted:
#' excdfval <- pcop(excop, eva = TRUE, u = c(0.5, 0.5))
#' }
#'
#' @export

pcop <- function(copula, eva, u){
  UseMethod("pcop")
}