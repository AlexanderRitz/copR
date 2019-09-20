#' Generating random samples of a Frank copula based on the algorithm of
#' Marshall and Olkin (1988)
#'
#' @inheritParams rCop
#'
#' @export


rCop.frankCop <- function (copula = NULL, n = 0) {
  if (n <= 0) {
    stop("Sample size has to be greater than 0")
  }
  theta <- copula$parameter
  d <- copula$dim
  if (d == 2) {
    if (theta > 0) {
      theta <- -theta
      U1 <- stats::runif(n = n, min = 0, max = 1)
      U2 <- stats::runif(n = n, min = 0, max = 1)
      U2 <- (-1 / theta * log1p(-U2 * expm1(-theta) /
          (exp(-theta * U1) * (U2 - 1) - U2)))
      U <- cbind(U1, (1 - U2))
      return(U)
    } else {
      U1 <- stats::runif(n = n, min = 0, max = 1)
      U2 <- stats::runif(n = n, min = 0, max = 1)
      U2 <- (-1 / theta * log1p(-U2 * expm1(-theta) /
          (exp(-theta * U1) * (U2 - 1) - U2)))
      U <- cbind(U1, U2)
      return(U)
    }
  } else {
    # Constructing a Logarithmic Series distributed RV
    theta <- (1 - exp(-theta))
    U1 <- stats::runif(n = n, min = 0, max = 1)
    V <- c()
    q <- NULL
    for (i in 1:n) {
      if (U1[i] >= theta) {
        V[i] <- 1
      } else {
        U2 <- stats::runif(n = 1,
          min = 0,
          max = 1)
        q <- 1 - exp(log(1 - theta) * U2)
        if (U1[i] <= q ^ 2) {
          V[i] <- floor(1 + (log(U1[i]) / log(q)))
        } else if (q ^ 2 < U1[i] && U1[i] <= q) {
          V[i] <- 1
        } else if (U1[i] > q) {
          V[i] <- 2
        }
      }
    }

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
}
