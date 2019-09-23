#' Density function of a Copula
#'
#' Constructs the density function by differntiating the supplied cdf expression.
#'
#' @param copula A copula object. Supplies the copula for which the pdf is to be
#' constructed.
#' @param u double. Optional argument supplying the data to evaluate the pdf.
#' @return expression or numeric for the pdf of the supplied copula object or
#' its value at the supplied u.
#'
#' @references Hofert et al. (2018). Elements of Copula Modeling with R.
#' Springer.
#' @references
#' Nelsen (2006). An introduction to copulas. Springer Series in Statistics.
#' Second Edition.
#'
#' @examples
#' \donttest{
#' excop <- claycop(par = 5, dim = 2)
#' expdf <- dcop(excop, eva = FALSE)
#' #In case evaluation of the pdf is wanted:
#' expdfval <- dcop(excop, eva = TRUE, u = c(0.5, 0.5))
#' }
#'
#' @export
#' @seealso \code{\link{pcop}} and \code{\link{rcop}}

dcop <- function(copula, u){
  UseMethod("dcop")
}
