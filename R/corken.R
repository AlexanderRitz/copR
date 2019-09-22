#' Kendall's Tau (empirical)
#'
#' Wrapper function for computing all pairwise Kendall rank correlation
#' coefficients between the columns of a given data matrix.
#'
#' @param data double. Supplies the data as a matrix or data frame.
#' @param use character. String (may be abbreviated) indicating the treatment of
#' missing values: "everything", "all.obs", "complete.obs", "na.or.complete", or
#' "pairwise.complete.obs" (see \code{cor} for an explanation of the options).
#' @param fast logical. For large data sets without missing values, the
#' computation of Kendall's tau can be sped up considerably by using the
#' function \code{cor.fk} from the package \code{pcaPP}.
#'
#' @return Matrix with pairwise Kendall rank correlation coefficients between
#' the columns of the supplied data.
#'
#' @examples
#' \donttest{
#' # standard usage
#' iris <- iris[, -5]
#' corken(iris)
#'
#' # with missing values
#' iris[1,1] <- NA
#' corken(iris)
#' corken(iris, use = "complete.obs")
#' corken(iris, fast = TRUE)
#'
#' # fast
#' corken(swiss, fast = TRUE)
#' }
#'
#' @export

corken <- function (data,
                    use = "everything",
                    fast = FALSE) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("The data needs to be supplied in a matrix or data frame.")
  }
  if (!is.numeric(data)) {
    stop("The supplied data must be numeric.")
  }
  if (isTRUE(fast)) {
    if (anyNA(data)) {
      stop("The fast computation function cor.fk() from the pcaPP package does
           not work if any values in the data are missing.")
    }
    if (!requireNamespace("pcaPP", quietly = TRUE)) {
      stop("Please install the pcaPP package in order to be able to use the fast
           computation option.")
    }
    pcaPP::cor.fk(data)
  } else {
    stats::cor(data, use = use, method = "kendall")
  }
}
