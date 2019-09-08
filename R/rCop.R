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
#' exS <- rCop(copula = exC, n = 1000)
#' plot(exS, ylim = c(0, 1), xlim = c(0, 1), ylab = "U2", xlab = "U1")
#' }
#'
#' @export

rCop <- function (copula = NULL, n = 0) {

  if (class(copula) == "ArCop"){

    if (copula$Name == "Clayton") {
      theta <- copula$par
      d <- copula$dim
      V <- stats::rgamma(n = n, shape = 1 / theta, scale = 1)
    } else {
      stop("Given family of copula not implemented. Please check availabe families by looking up the allowed Types.")
    }
    X <- matrix(data = NA, nrow = n, ncol = d)

    for (i in 1:d) {
      X[, i] <- stats::runif(n = n, min = 0, max = 1)
    }
    U <- matrix(data = NA, nrow = n, ncol = d)

    for (j in 1:d) {

      for (i in 1:n) {
        U[i, j] <- InvGenEval(s = (-log(X[i, j]) / V[i]), copula = copula)
      }
    }
    return(U)

  } else {
    stop("Supplied copula is not of class ArCop, please supply an appropriate object.")
  }
}
