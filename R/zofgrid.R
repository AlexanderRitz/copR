#' Utility to construct xyz-coordinates of a function for plotting functions
#'
#' Sets up an xy-grid, has the supplied function evaluated on the grid, and
#' returns the resulting xyz-coordinates.
#'
#' @param cop A copula object.
#' @param FUN character. Decides whether the cdf or pdf of the supplied copula
#' is to be plotted. Accepts "cdf" or "pdf" as input.
#' @param n integer. Number of grid points in x- and y-direction.
#' @param delta numeric. A value in the range [0, 0.5) for changing the
#' evaluation boundaries by adding it to the lower limits of x and y and
#' subtracting it from the upper limits of x and y.
#' @param xlim integer. Range of x values to be plotted.
#' @param ylim integer. Range of y values to be plotted.
#' @return A list with a vector  of x-coordinates, a vector of y-coordinates and
#' a matrix with the function values of FUN on the xy-grid.

zofgrid <- function(cop, FUN, n, delta, xlim, ylim) {
  gx <- seq(xlim[1] + delta, xlim[2] - delta, length.out = n[1])
  gy <- seq(ylim[1] + delta, ylim[2] - delta, length.out = n[2])
  theta <- cop$parameter
  if (FUN == "cdf") {
    z <- app(x = gx, y = gy, f = cop$distribution$cdf, par = theta)
  } else if (FUN == "pdf") {
    z <- app(x = gx,  y = gy, f = cop$distribution$pdf, par = theta)
  } else {
    stop(
      "Please choose between plotting either the cdf or pdf of the copula."
    )
  }
  return(list(gx = gx, gy = gy, z = z))
}