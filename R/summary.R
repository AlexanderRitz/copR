#' Summary of copula objects
#'
#' @inheritParams base::summary
#' @param force logical. If "TRUE", the cdf and pdf of the copula object will be
#' included despite their length.
#'
#' @export


summary.copula <- function(object, ..., force = FALSE) {
  if (is.indcop(object) == TRUE) {
    if (force == FALSE) {
      cat(paste(
        object$family,
        " copula: \n\t",
        "Dimensions: \t\t",
        object$dimension,
        sep = ""
      ))
    } else {
      cat(
        paste(
          object$family,
          " copula: \n\t",
          "Dimensions: \t\t",
          object$dimension,
          "\n\tcdf: \t",
          object$distribution$cdf,
          "\n\tpdf: \t",
          object$distribution$pdf,
          sep = ""
        )
      )
    }
  } else if (force == FALSE) {
    cat(
      paste(
        object$family,
        " copula: \n\t",
        "Dimensions: \t\t",
        object$dimension,
        "\n\tParameter value: \t",
        object$parameter,
        "\n\tParameter range: \t",
        "(",
        object$prange[1],
        ", ",
        object$prange[2],
        ")\n\t",
        "Generator function: \t",
        object$generator$gen,
        "\n\tInverse generator: \t",
        object$generator$invg,
        sep = ""
      )
    )
  } else {
    cat(
      paste(
        object$family,
        " copula: \n\t",
        "Dimensions: \t\t",
        object$dimension,
        "\n\tParameter value: \t",
        object$parameter,
        "\n\tParameter range: \t",
        "(",
        object$prange[1],
        ", ",
        object$prange[2],
        ")\n\t",
        "Generator function: \t",
        object$generator$gen,
        "\n\tInverse generator: \t",
        object$generator$invg,
        "\n\tcdf: \t",
        object$distribution$cdf,
        "\n\tpdf: \t",
        object$distribution$pdf,
        sep = ""
      )
    )
  }
}


#' Summary of fitted copula objects
#'
#' @inheritParams base::summary
#' @param force logical. If "TRUE", the cdf and pdf of the copula object will be
#' included despite their length.
#'
#' @export


summary.fitcop <- function(object, ..., force = FALSE) {
  if (force == FALSE) {
    cat(
      paste(
        object$family,
        " copula: \n\t",
        "Dimensions: \t\t",
        object$dimension,
        "\n\tMax. likelihood est.: \t",
        object$parameter,
        "\n\tParameter range: \t",
        "(",
        object$prange[1],
        ", ",
        object$prange[2],
        ")\n\t",
        "Log-likelihood: \t",
        object$loglikelihood,
        "\n\tGenerator function: \t",
        object$generator$gen,
        "\n\tInverse generator: \t",
        object$generator$invg,
        sep = ""
      )
    )
  } else {
    cat(
      paste(
        object$family,
        " copula: \n\t\t",
        "Dimensions: \t\t",
        object$dimension,
        "\n\tMax. likelihood est.: \t",
        object$parameter,
        "\n\tParameter range: \t",
        "(",
        object$prange[1],
        ", ",
        object$prange[2],
        ")\n\t",
        "Log-likelihood: \t",
        object$loglikelihood,
        "\n\tGenerator function: \t",
        object$generator$gen,
        "\n\tInverse generator: \t",
        object$generator$invg,
        "\n\tcdf: \t",
        object$distribution$cdf,
        "\n\tpdf: \t",
        object$distribution$pdf,
        sep = ""
      )
    )
  }
}
