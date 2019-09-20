#' Construction of Clayton cdf
#'
#' @inheritParams pCop
#'
#' @export

pCop.clayCop <- function (copula, eva = FALSE, u) {
  if (is.null(copula$distribution$cdf)) {
    d <- copula$dimension
    theta <- copula$parameter
    expr <- "u1^(-theta) - 1"
    for (i in 2:d) {
      expr2 <- paste("u", i, "^(-theta) - 1", sep = "")
      expr <- paste(expr, expr2, sep = " + ")
    }
    expr <- paste("(1 + (", expr, "))^(-1/theta)")
    if (eva == FALSE) {
      parse(text = expr)
    } else {
      if (length(u) == d) {
        u[u >= 1] <- 1
        u[u <= 0] <- 0
        for (i in 1:d) {
          assign(paste("u", i, sep = ""), u[i])
        }
        theta <- copula$parameter
        eval(parse(text = expr))
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
    if (eva == FALSE) {
      return(expr)
    } else if (length(u) == d) {
      u[u >= 1] <- 1
      u[u <= 0] <- 0
      for (i in 1:d) {
        assign(paste("u", i, sep = ""), u[i])
      }
      theta <- copula$parameter
      eval(expr)
    } else {
      stop(
        "Supplied data vector not of appropriate length. Has to be of the same
        dimension as the supplied copula."
      )
    }
  }
}
