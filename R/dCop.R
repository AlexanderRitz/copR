#' Density function of a Copula
#'
#' Constructs the density function by differntiating the supplied cdf expression.
#'
#' @param copula ArCop object. Supplies the copula for which the pdf is to be constructed.
#' @return An expression for the pdf of the supplied copula.
#'
#' @examples
#' \donttest{
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exS <- rCop(copula = exC, n = 1000)
#' }
#'
#' @export

dCop <- function(copula){
  d <- copula$dimension
  if (is.null(copula$distribution$cdf)) {
    stop("Supplied copula object does not contain a cdf expresssion")
  } else {
    pdf <- copula$distribution$cdf
    for (i in 1:d){
      pdf <- stats::D(pdf, paste0("u", i))
    }
    return(pdf)
  }
}
