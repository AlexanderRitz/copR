#' Density function of a Copula
#'
#' Constructs the density function by differntiating the supplied cdf expression.
#'
#' @param cdf expression. Supplies the cdf for which the pdf is to be calculated.
#' @return An expression for the pdf of the supplied cdf.
#'
#' @examples
#' \donttest{
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exS <- rCop(copula = exC, n = 1000)
#' }
#'
#' @export
