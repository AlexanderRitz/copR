#' Construction of Clayton pdf
#'
#' @inheritParams dCop
#'
#' @export

dCop.clayCop <- function(copula, eva, u){
  d <- copula$dimension
  theta <- copula$parameter
  if (is.null(copula$distribution$cdf)) {
    stop("Supplied copula object does not contain a cdf expresssion")
  } else {
    pdf <- copula$distribution$cdf
    for (i in 1:d){
      pdf <- stats::D(pdf, paste("u", i, sep = ""))
    }
    if (eva == FALSE) {
    return(pdf)
    } else {
      if (length(u) == d) {
        for (i in 1:d) {
          assign(paste("u", i, sep = ""), u[i])
        }
        eval(pdf)
      } else {
        stop(
          "Supplied data vector not of appropriate length. Has to be of the same dimension as the supplied copula."
        )
      }
    }
  }
}
