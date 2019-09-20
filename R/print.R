#' Printing of copula objects
#'
#' @inheritParams base::print
#' @param force logical. If "TRUE", the cdf and pdf of the copula object will be
#' printed despite its length.
#'
#' @export



print.copula <- function (x, force = FALSE, ...) {
  if (is.indCop(x) == TRUE) {
    if (x$dimension < 10) {
      cat(
        paste(
          "An ",
          x$family,
          " copula in ",
          x$dimension,
          " dimensions.",
          sep = ""
        ),
        "\n",
        paste("cdf = ", as.character(x$distribution$cdf) , sep = ""),
        "\n",
        paste("pdf = ", as.character(x$distribution$pdf) , sep = "")
      )
    } else {
      cat(
        paste(
          "An ",
          x$family,
          " copula in ",
          x$dimension,
          " dimensions.",
          sep = ""
        ),
        "\n",
        paste("cdf omitted due to length"),
        "\n",
        paste("pdf omitted due to length")
      )
    }
  } else if (force == TRUE) {
    cat(
      paste(
        "A ",
        x$family,
        " copula in ",
        x$dimension,
        " dimensions with parameter theta = ",
        x$parameter,
        ".",
        sep = ""
      ),
      "\n",
      paste("cdf = ", as.character(x$distribution$cdf) , sep = ""),
      "\n",
      paste("pdf = ", as.character(x$distribution$pdf) , sep = "")
    )
  } else {
    cat(
      paste(
        "A ",
        x$family,
        " copula in ",
        x$dimension,
        " dimensions with parameter theta = ",
        x$parameter,
        ".",
        sep = ""
      ),
      "\n",
      paste("cdf omitted due to length"),
      "\n",
      paste("pdf omitted due to length")
    )
  }
}
