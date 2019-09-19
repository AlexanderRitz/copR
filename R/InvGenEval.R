#' Inverse Generator function utility
#'
#' Applies appropriate inverted generator function to input, based on supplied copula object.
#'
#' @param copula A copula object. Decides form of generator function.
#' @param s numeric. Input for the inverted generator function to be applied.
#' @return The value of the inverse generator function of the given copula object at input x.
#'

invGenEval <- function (s, copula) {
  if (length(copula$parameter == 1L)) {
    param <- copula$parameter
    theta <- param
    out <- eval(copula$expressions$invg[[1]])
    return(out)

  } else {
    stop(
      "Families with more than one parameter are not supported at the moment."
    )
  }
}
