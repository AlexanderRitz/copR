#' Sampling of Copula
#'
#' Constructs a sample based on a given copula object.
#'
#' @param copula A copula object. Decides the copula to sample from.
#' @param n integer. Supplies the desired number of observations.
#' @return A matrix containing n observations of d random variables, based on
#' the family of the supplied copula object.
#'
#' @references Hofert et al. (2018). Elements of Copula Modeling with R.
#' Springer.
#' @references
#' Nelsen (2006). An introduction to copulas. Springer Series in Statistics.
#' Second Edition.
#' @references Whelan (2004). Sampling from Archimedean copulas. Quantitative
#' Finance, 2004, vol. 4, issue 3, pp. 339-352.
#' @references Marshall and Olkin (1988). Families of Multivariate Distributions.
#' Journal of the American Statistical Association, Vol. 83, No. 403, pp.834-841.
#' @references
#' Genest (1987). Frank's family of bivariate distributions. Biometrika (1987),
#' 74, 3, pp. 549-55.
#'
#' @examples
#' \donttest{
#' excop <- Claycop(par = 5, dim = 2)
#' exsam <- rcop(copula = excop, n = 1000)
#' plot(exsam, ylim = c(0, 1), xlim = c(0, 1), ylab = "U2", xlab = "U1")
#' }
#'
#' @export
#' @seealso \code{\link{pcop}} and \code{\link{dcop}}

rcop <- function (copula = NULL, n = 0) {
  UseMethod("rcop")
}
