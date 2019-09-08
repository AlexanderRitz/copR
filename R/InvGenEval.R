#' Inverse Generator function utility
#'
#' Applies appropriate inverted generator function to input, based on supplied ArCop object.
#'
#' @param copula ArCop object. Decides form of generator function.
#' @param s numeric. Input for the inverted generator function to be applied.
#' @return The value of the inverse generator function of the given ArCop object at input x.
#'

InvGenEval <- function (s, copula) {

  if (length(copula$parameters == 1L)) {
    param <- copula$parameters
    theta <- param
    out <- eval(copula$expressions$invg[[1]])
    return(out)

  } else {
    warning("In case of self built copula object with more than one parameter, correct order of parameters should be checked!")
  }
}
