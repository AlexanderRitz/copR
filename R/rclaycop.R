#' Generating random samples of a Clayton copula based on the algorithm of
#' Marshall and Olkin (1988)
#'
#' @inheritParams rcop
#'
#' @export

rcop.claycop <- function (copula = NULL, n = 0) {
  if (n <= 0) {
    stop(
      "Sample size has to be greater than 0"
      )
  }
  theta <- copula$par
  d <- copula$dim
  V <- stats::rgamma(n = n,
                     shape = 1 / theta,
                     scale = 1)
  X <- matrix(data = NA,
              nrow = n,
              ncol = d)

  for (i in 1:d) {
    X[, i] <- stats::runif(n = n, min = 0, max = 1)
  }
  U <- matrix(data = NA,
              nrow = n,
              ncol = d)

  for (j in 1:d) {
    for (i in 1:n) {
      U[i, j] <- invgeneval(s = (-log(X[i, j]) / V[i]), copula = copula)
    }
  }
  return(U)
}
