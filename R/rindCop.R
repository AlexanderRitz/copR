#' Generating random samples of an Independence copula
#'
#' @inheritParams rcop
#'
#' @export

rcop.indcop <- function (copula = NULL, n = 0) {
  if (n <= 0) {
    stop(
      "Sample size has to be greater than 0"
      )
  }
  d <- copula$dim
  U <- matrix(NA, nrow = n, ncol = d)
  for (i in 1:d) {
    U[, i] <- stats::runif(n = n, min = 0, max = 1)
  }
  return(U)
}
