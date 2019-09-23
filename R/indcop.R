#' Construction of Independence copula object
#'
#' Constructs a indcop object, holding all relevant information on an
#' Independence copula. The number of dimensions has to be supplied.
#'
#' @param dim integer. Supplies the number of dimensions of the copula to be
#' constructed.
#' @return A list of class "indcop" with elements
#' \item{dimension}{Number of dimensions}
#' \item{family}{Name of constructed copula family, here "Independence"}
#' \item{distribution}{A list consisting of expressions for cdf and pdf}
#' The expressions can be evaluated for the variables "ui" in the case of cdf
#' and pdf.
#'
#' @references Hofert et al. (2018). Elements of Copula Modeling with R.
#' Springer.
#' @references
#' Nelsen (2006). An introduction to copulas. Springer Series in Statistics.
#' Second Edition.
#' @references
#' Frees and Valdez (1998). Understanding Relationships Using Copulas. North
#' American Actuarial Journal 2(1):1-25 January 1998.
#'
#' @examples
#' \donttest{
#' excop <- indcop(dim = 2)
#' summary(excop)
#' }
#'
#' @export
#' @seealso \code{\link{claycop}} and \code{\link{frankcop}}


indcop <- function (dim = 2L) {
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
    class(result) <- c("indcop", "copula")
    cdf <- pcop(result)
    result$distribution$cdf <- cdf
    pdf <- dcop(result)
    result$distribution$pdf <- pdf

    return(result)
  }
}
