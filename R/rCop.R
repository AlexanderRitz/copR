#' Sampling of Copula
#'
#' Constructs a sample based on a given ArCop object.
#'
#' @param copula ArCop object. Decides the copula to sample from.
#' @param n integer. Supplies the desired number of observations.
#' @return A matrix containing n observations of d random variables, based on the "Type" of the supplied ArCop object.
#'
#' @examples
#' \donttest{
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exS <- rCop(copula = ex, n = 1000)
#' plot(exS, ylim = c(0, 1), xlim = c(0, 1), ylab = "U2", xlab = "U1")
#' }
#'
#' @export

rCop <- function(copula = NULL, n = 0){

  if(copula$Name == "Clayton"){

    theta <- copula$par
    d <- copula$dim

    V <- stats::rgamma(n = n, shape = 1 / theta, scale = 1)

    X <- matrix(data = NA, nrow = n, ncol = d)
    for (i in 1:d) {
      X[, i] <- stats::runif(n = n, min = 0, max = 1)
    }

    U <- matrix(data = NA, nrow = n, ncol = d)
    for (j in 1:d) {
      for (i in 1:n) {
        U[i, j] <- (1 + (- log(X[i, j]) / V[i])) ^ (-(1 / theta))
      }
    }

    return(U)
  }
}
