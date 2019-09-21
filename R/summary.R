#' Summary of copula objects
#'
#' @inheritParams base::summary
#' @param force logical. If "TRUE", the cdf and pdf of the copula object will be
#' included despite their length.
#'
#' @export


summary.copula <- function(x, forced = FALSE) {
  if (is.indcop(x) == TRUE) {
    if (forced == FALSE) {
      cat(paste(
        x$family,
        " copula: \n\t",
        "Dimensions: \t\t",
        x$dimension,
        sep = ""
      ))
    } else {
      cat(
        paste(
          x$family,
          " copula: \n\t",
          "Dimensions: \t\t",
          x$dimension,
          "\n\tcdf: \t",
          x$distribution$cdf,
          "\n\tpdf: \t",
          x$distribution$pdf,
          sep = ""
        )
      )
    }
  } else if (forced == FALSE) {
    cat(
      paste(
        x$family,
        " copula: \n\t",
        "Dimensions: \t\t",
        x$dimension,
        "\n\tParameter value: \t",
        x$parameter,
        "\n\tParameter range: \t",
        "(",
        x$prange[1],
        ", ",
        x$prange[2],
        ")\n\t",
        "Generator function: \t",
        x$generator$gen,
        "\n\tInverse generator: \t",
        x$generator$invg,
        sep = ""
      )
    )
  } else {
    cat(
      paste(
        x$family,
        " copula: \n\t",
        "Dimensions: \t\t",
        x$dimension,
        "\n\tParameter value: \t",
        x$parameter,
        "\n\tParameter range: \t",
        "(",
        x$prange[1],
        ", ",
        x$prange[2],
        ")\n\t",
        "Generator function: \t",
        x$generator$gen,
        "\n\tInverse generator: \t",
        x$generator$invg,
        "\n\tcdf: \t",
        x$distribution$cdf,
        "\n\tpdf: \t",
        x$distribution$pdf,
        sep = ""
      )
    )
  }
}
