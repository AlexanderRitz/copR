#' Construction of Independence cdf
#'
#' @inheritParams pcop
#'
#' @export

pcop.indcop <- function (copula, u = NULL) {
  if (is.null(copula$distribution$cdf)) {
    d <- copula$dimension
    expr <- "u1"
    for (i in 2:d) {
      expr2 <- paste("u", i, sep = "")
      expr <- paste(expr, expr2, sep = " * ")
    }
    if (is.null(u)) {
      parse(text = expr)
    } else {
      if (length(u) == d) {
        if (!any(is.na(u))) {
          u[u >= 1] <- 1
          u[u <= 0] <- 0
          for (i in 1:d) {
            assign(paste("u", i, sep = ""), u[i])
          }
          eval(parse(text = expr))
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
  } else {
    expr <- copula$distribution$cdf
    d <- copula$dimension
    if (is.null(u)) {
      return(expr)
    } else if (length(u) == d) {
      if (!any(is.na(u))) {
        u[u >= 1] <- 1
        u[u <= 0] <- 0
        for (i in 1:d) {
          assign(paste("u", i, sep = ""), u[i])
        }
        eval(expr)
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
