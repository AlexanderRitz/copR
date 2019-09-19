#' Density function of a Copula
#'
#' Constructs the density function by differntiating the supplied cdf expression.
#'
#' @param copula ArCop object. Supplies the copula for which the pdf is to be constructed.
#' @param eva Logical. If eva = TRUE, the resulting expression will be evaluated for the supplied arguments.
#' @param u numeric. Optional argument supplying the data to evaluate the pdf, only needed in case of eva = TRUE.
#' @return expression or numeric for the pdf of the supplied copula object or its value at the supplied u.
#'
#' @examples
#' \donttest{
#' exCop <- clayCop(par = 5, dim = 2)
#' exPDF <- dCop(exCop, eva = FALSE)
#' #In case evaluation of the pdf is wanted:
#' exPDFval <- dCop(exCop, eva = TRUE, u = c(0.5, 0.5))
#' }
#'
#' @export

dCop <- function(copula, eva, u){
  UseMethod("dCop")
}
