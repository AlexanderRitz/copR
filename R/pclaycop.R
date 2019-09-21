#' Construction of Clayton cdf
#'
#' @inheritParams pcop
#'
#' @export

pcop.claycop <- function (copula, u = NULL) {
  if (is.null(copula$distribution$cdf)) {
    d <- copula$dimension
    theta <- copula$parameter
    expr <- "u1^(-theta) - 1"
    for (i in 2:d) {
      expr2 <- paste("u", i, "^(-theta) - 1", sep = "")
      expr <- paste(expr, expr2, sep = " + ")
    }
    expr <- paste("(1 + (", expr, "))^(-1/theta)")
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
          theta <- copula$parameter
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
        theta <- copula$parameter
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
