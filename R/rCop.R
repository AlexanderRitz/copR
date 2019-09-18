#' Sampling of Copula
#'
#' Constructs a sample based on a given copula object.
#'
#' @param copula A copula object. Decides the copula to sample from.
#' @param n integer. Supplies the desired number of observations.
#' @return A matrix containing n observations of d random variables, based on the family of the supplied copula object.
#'
#' @examples
#' \donttest{
#' exCop <- ClayCop(par = 5, dim = 2)
#' exSam <- rCop(copula = exCop, n = 1000)
#' plot(exSam, ylim = c(0, 1), xlim = c(0, 1), ylab = "U2", xlab = "U1")
#' }
#'
#' @export

rCop <- function (copula = NULL, n = 0) {
  UseMethod("rCop")
}
