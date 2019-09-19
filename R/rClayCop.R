#' Generating random samples of a Clayton copula based on the algorithm of
#' Marshall and Olkin (1988)
#'
#' @inheritParams rCop
#'
#' @export

rCop.clayCop <- function (copula = NULL, n = 0) {
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
      U[i, j] <- invGenEval(s = (-log(X[i, j]) / V[i]), copula = copula)
    }
  }
  return(U)
}
