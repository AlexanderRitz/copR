#' Construction of Independence cdf
#'
#' @inheritParams pCop
#'
#' @export

pCop.indCop <- function (copula, eva = FALSE, u) {
  if (is.null(copula$distribution$cdf)) {
    d <- copula$dimension
    expr <- "u1"
    for (i in 2:d) {
      expr2 <- paste("u", i, sep = "")
      expr <- paste(expr, expr2, sep = " * ")
    }
    if (eva == FALSE) {
      parse(text = expr)
    } else {
      if (length(u) == d) {
        u[u >= 1] <- 1
        u[u <= 0] <- 0
        for (i in 1:d) {
          assign(paste("u", i, sep = ""), u[i])
        }
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
      eval(expr)
    } else {
      stop(
        "Supplied data vector not of appropriate length. Has to be of the same
        dimension as the supplied copula."
      )
    }
  }
}
