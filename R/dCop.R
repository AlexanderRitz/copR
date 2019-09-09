#' Density function of a Copula
#'
#' Constructs the density function for a supplied copula object.
#'
#' @param copula ArCop object. Supplies the copula for which the pdf is to be defined.
#' @return An expression for the pdf of the supplied copula object.
#'
#' @examples
#' \donttest{
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exS <- rCop(copula = exC, n = 1000)
#' }
#'
#' @export
