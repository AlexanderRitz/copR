#' Printing of copula objects
#'
#' @inheritParams base::print
#' @param force logical. If "TRUE", the cdf and pdf of the copula object will be
#' printed despite their length.
#'
#' @export


print.copula <- function (x, force = FALSE, ...) {
  if (is.indcop(x) == TRUE) {
    if (x$dimension < 10 || force == TRUE) {
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
        "\n",
        paste(
          "cdf omitted due to length, access with \"yourcopula\"$distribution$cdf"
          ),
        "\n",
        paste(
          "pdf omitted due to length, access with \"yourcopula\"$distribution$pdf"
          )
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
      "\n",
      paste(
        "pdf omitted due to length, access with \"yourcopula\"$distribution$cdf"
        ),
      "\n",
      paste(
        "pdf omitted due to length, access with \"yourcopula\"$distribution$pdf"
        )
    )
  }
}


#' Printing of fitted copula objects
#'
#' @inheritParams base::print
#'
#' @export

print.fitcop <- function (x, ...) {
  cat(
    paste(
      "The ",
      x$dimension,
      " dimensional ",
      x$family,
      " copula has been fitted with theta = ",
      x$parameter,
      ", based on ",
      x$observations,
      " observations.",
      sep = ""
    ),
    paste(
      "\n\nThe maximum value of the log-likelihood function was ",
      x$loglikelihood,
      ".",
      sep = ""
    )
  )
}
