#' Construction of Independence copula object
#'
#' Constructs a indCop object, holding all relevant information on an
#' Independence copula. The number of dimensions has to be supplied.
#'
#' @param dim integer. Supplies the number of dimensions of the copula to be
#' constructed.
#' @return A list of class "indCop" with elements
#' \item{dimension}{Number of dimensions}
#' \item{family}{Name of constructed copula family, e.g. "Clayton"}
#' \item{distribution}{A list consisting of expressions for cdf and pdf}
#' The expressions can be evaluated for the variables "ui" in the case of cdf
#' and pdf.
#'
#' @examples
#' \donttest{
#' exCop <- indCop(dim = 2)
#' summary(exCop)
#' }
#'
#' @export


indCop <- function (dim = 2L) {
  if (length(dim) != 1) {
    stop(
      "Dimension not of correct format, please supply a single integer."
    )
  } else if (dim < 2) {
    stop(
      "A copula can only be constructed for at least 2 dimensions!"
    )
  } else {
    fam <- "Independence"
    cdf <- NULL
    pdf <- NULL
    result <- list(
      dimension = dim,
      family = fam,
      distribution = list(cdf = cdf, pdf = pdf)
    )
    class(result) <- c("indCop", "copula")
    cdf <- pCop(result, eva = FALSE)
    result$distribution$cdf <- cdf
    pdf <- dCop(result, eva = FALSE)
    result$distribution$pdf <- pdf

    return(result)
  }
}
