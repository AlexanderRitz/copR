#' Inverse Generator function utility
#'
#' Applies appropriate inverted generator function to input, based on supplied
#' copula object.
#'
#' @param copula A copula object. Decides form of generator function.
#' @param s double. Input for the inverted generator function to be applied.
#' @return The value of the inverse generator function of the given copula
#' object at input x.
#'
#' @references Hofert et al. (2018). Elements of Copula Modeling with R.
#' Springer.
#' @references
#' Frees and Valdez (1998). Understanding Relationships Using Copulas. North
#' American Actuarial Journal 2(1):1-25. January 1998
#'

invgeneval <- function (s, copula) {
  if (length(copula$parameter == 1L)) {
    theta <- copula$parameter
    out <- eval(copula$generator$invg[[1]])
    return(out)

  } else {
    stop(
      "Families with more than one parameter are not supported at the moment."
    )
  }
}
