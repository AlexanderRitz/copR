#' Construction of Independence pdf
#'
#' @inheritParams dCop
#'
#' @export

dCop.indCop <- function(copula, eva, u) {
  if (is.null(copula$distribution$pdf)) {
    d <- copula$dimension
    if (is.null(copula$distribution$cdf)) {
      stop(
        "Supplied copula object does not contain a cdf expresssion"
      )
    } else {
      pdf <- copula$distribution$cdf
      for (i in 1:d) {
        pdf <- stats::D(pdf, paste("u", i, sep = ""))
      }
      if (eva == FALSE) {
        return(pdf)
      } else {
        if (length(u) == d) {
          if (any(u >= 1) || any(u <= 0)) {
            return(0)
          }
          for (i in 1:d) {
            assign(paste("u", i, sep = ""), u[i])
          }
          eval(pdf)
        } else {
          stop(
            "Supplied data vector not of appropriate length. Has to be of the
            same dimension as the supplied copula."
          )
        }
      }
    }
  } else if (eva == FALSE) {
    return(copula$distribution$pdf)
  } else {
    d <- copula$dimension
    if (length(u) == d) {
      if (any(u >= 1) || any(u <= 0)) {
        return(0)
      } else {
        for (i in 1:d) {
          assign(paste("u", i, sep = ""), u[i])
        }
        eval(copula$distribution$pdf)
      }
    } else {
      stop(
        "Supplied data vector not of appropriate length. Has to be of the
            same dimension as the supplied copula."
      )
    }
  }
}
