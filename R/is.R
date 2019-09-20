#' Utility to check inheritance of copula objects for "clayCop"
#'
#' @param copula A copula object.
#' @return TRUE or FALSE depending on whether the supplied copula object is of
#' the class "claycop".
#'

is.clayCop <- function (copula) {
  inherits(copula, "clayCop")
}

#' Utility to check inheritance of copula objects for "frankCop"
#'
#' @param copula A copula object.
#' @return TRUE or FALSE depending on whether the supplied copula object is of
#' the class "frankcop".
#'

is.frankCop <- function (copula) {
  inherits(copula, "frankCop")
}

#' Utility to check inheritance of copula objects for "indCop"
#'
#' @param copula A copula object.
#' @return TRUE or FALSE depending on whether the supplied copula object is of
#' the class "indcop".
#'

is.indCop <- function (copula) {
  inherits(copula, "indCop")
}

#' Utility to check inheritance of objects for "copula"
#'
#' @param copula A copula object.
#' @return TRUE or FALSE depending on whether the supplied object is of
#' the class copula.
#'

is.copula <- function (copula) {
  inherits(copula, "copula")
}
