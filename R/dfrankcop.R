#' Construction of Frank pdf
#'
#' @inheritParams dcop
#'
#' @export




dcop.frankcop <- function(copula, u = NULL) {
  if (is.null(copula$distribution$pdf)) {
    d <- copula$dimension
    theta <- copula$parameter
    if (is.null(copula$distribution$cdf)) {
      stop("Supplied copula object does not contain a cdf expresssion")
    } else {
      pdf <- copula$distribution$cdf
      for (i in 1:d) {
        pdf <- stats::D(pdf, paste("u", i, sep = ""))
      }
      if (is.null(u)) {
        return(parse(
          text = paste(
            as.character(pdf)[2],
            as.character(pdf)[1],
            as.character(pdf)[3],
            sep = ""
          )
        ))
      } else {
        if (length(u) == d) {
          if (!any(is.na(u))) {
            if (any(u >= 1) || any(u <= 0)) {
              return(0)
            }
            for (i in 1:d) {
              assign(paste("u", i, sep = ""), u[i])
            }
            eval(pdf)
          } else {
            stop(
              "Supplied u contains missing values!"
              )
          }
        } else {
          stop(
            "Supplied data vector not of appropriate length. Has to be of the same
          dimension as the supplied copula."
          )
        }
      }
    }
  } else if (is.null(u)) {
    return(copula$distribution$pdf)
  } else {
    d <- copula$dimension
    if (length(u) == d) {
      if (!any(is.na(u))) {
        if (any(u >= 1) || any(u <= 0)) {
          return(0)
        }
        for (i in 1:d) {
          assign(paste("u", i, sep = ""), u[i])
        }
        theta <- copula$parameter
        eval(copula$distribution$pdf)
      } else {
        stop(
          "Supplied u contains missing values!"
          )
      }
    } else {
      stop(
        "Supplied data vector not of appropriate length. Has to be of the same
          dimension as the supplied copula."
      )
    }
  }
}
