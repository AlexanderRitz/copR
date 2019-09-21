#' Likelihood of Copula
#'
#' Calculates the Likelihood function of a chosen Copula family
#'
#' @param copula A copula object. Decides the copula family for which the
#' Likelhood function is to be calculated.
#' @param data The data to base the Likelihood on.
#' @param parameter Parameter value of the copula.
#' @return An expression of the calculated Likelihood function.
#'
#' @examples
#' \donttest{
#' exc <- claycop(par = 5, dim = 2)
#' exs <- rcop(copula = exc, n = 1000)
#' }
#'
#' @export

cloglik <- function (copula, data, parameter) {
  d <- copula$dimension
  n <- nrow(data)
  theta <- parameter
  ll <- 0
  for (i in 1:n) {
    ll <- ll + log(dcop(copula, eva = TRUE, u = data[i,]))
    print(ll)
  }
}
