#' Utility to check inheritance of copula objects for "claycop"
#'
#' @param copula A copula object.
#' @return TRUE or FALSE depending on whether the supplied copula object is of
#' the class "claycop".
#'

is.claycop <- function (copula) {
  inherits(copula, "claycop")
}

#' Utility to check inheritance of copula objects for "frankcop"
#'
#' @param copula A copula object.
#' @return TRUE or FALSE depending on whether the supplied copula object is of
#' the class "frankcop".
#'

is.frankcop <- function (copula) {
  inherits(copula, "frankcop")
}

#' Utility to check inheritance of copula objects for "indcop"
#'
#' @param copula A copula object.
#' @return TRUE or FALSE depending on whether the supplied copula object is of
#' the class "indcop".
#'

is.indcop <- function (copula) {
  inherits(copula, "indcop")
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
