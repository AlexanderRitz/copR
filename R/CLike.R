#' Likelihood of Copula
#'
#' Calculates the Likelihood function of a chosen Copula family
#'
#' @param copula A copula object. Decides the copula family for which the Likelhood function is to be calculated.
#' @param marg character. Supplied choices for the marginal distribution functions.
#' Can either be estimated through empirical functions or given by name.
#' @return An expression of the calculated Likelihood function.
#'
#' @examples
#' \donttest{
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exS <- rCop(copula = exC, n = 1000)
#' }
#'
#' @export

cLike <- function (copula, marg) {
  print("Initialisation")
}
