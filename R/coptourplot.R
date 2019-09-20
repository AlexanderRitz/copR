#' Contourplot of Copula
#'
#' Gives a contourplot of a given copula object for a specified function.
#'
#' @param copula A copula object. Supplies the copula to be used for the
#' contourplot.
#' @param FUN character. Decides whether the cdf or pdf of the supplied copula
#' is to be plotted. Accepts "cdf" or "pdf" as input.
#' @param n.grid integer. Number of grid points in x- and y-direction. Vector of
#' length two or one (used for both directions).
#' @param delta numeric. A value in the range [0, 0.5) for changing the
#' evaluation boundaries by adding it to the lower limits of x and y and
#' subtracting it from the upper limits of x and y. Defaults to zero.
#' @param nlevels integer. Number of contour levels to be plotted.
#' @param xlim integer. Range of x values to be plotted. Defaults to [0, 1].
#' @param ylim integer. Range of y values to be plotted. Defaults to [0, 1].
#' @param xlab character. Title for x-axis.
#' @param ylab character. Title for y-axis.
#' @param col.pal character. Name of the color palette to be used for the contour
#' levels, provided by the function hcl.colors() in the grDevices package.
#' @param con.lines logical. Draw contour lines if TRUE (default).
#' @param ... character, function, or integer. Additional graphical parameters.
#' @return A contourplot with the values given by FUN for a copula of the form
#' given by the supplied copula object.
#'
#' @examples
#' \donttest{
#' exCop <- clayCop(par = 0.5, dim = 2)
#' # Plotting the cdf
#' coPtourplot(copula = exCop, FUN = "cdf")
#' # Plotting the pdf
#' coPtourplot(copula = exCop, FUN = "pdf", n.grid = 42, nlevels = 16,
#'             col.pal = "YlGnBu",
#'             plot.title = title(main = "Clayton Copula PDF"))
#' }
#'
#' @export

coPtourplot <- function (copula,
                         FUN,
                         n.grid = 26,
                         delta = 0,
                         nlevels = 20,
                         xlim = 0:1,
                         ylim = 0:1,
                         xlab = quote(u[1]),
                         ylab = quote(u[2]),
                         col.pal = "Viridis",
                         con.lines = TRUE,
                         ...) {
  if (copula$dimension == 2 & n.grid >= 2) {
    if (length(n.grid) == 1) {
      n.grid <- rep(n.grid, 2)
    }
    if (length(n.grid) == 2) {
      if (0 <= delta && delta < (1 / 2)) {

        # Create xy-grid and calculate corresponding z-values
        gx <- seq(xlim[1] + delta, xlim[2] - delta, length.out = n.grid[1])
        gy <- seq(ylim[1] + delta, ylim[2] - delta, length.out = n.grid[2])
        theta_ <- copula$parameter
        if (FUN == "cdf") {
          z <- app(x = gx, y = gy, f = copula$distribution$cdf, par = theta_)
        } else if (FUN == "pdf") {
          z <- app(x = gx,  y = gy, f = copula$distribution$pdf, par = theta_)
        } else {
          stop("Please choose between plotting either the cdf or pdf of the
               copula.")
        }

        # Draw the contourplot
        if (con.lines == TRUE) {
          graphics::filled.contour(x = gx,
                                   y = gy,
                                   z = z,
                                   nlevels = nlevels,
                                   color.palette = function(n)
                                     grDevices::hcl.colors(n, col.pal),
                                   xlab = xlab,
                                   ylab = ylab,
                                   key.title = graphics::title(main = " Contour",
                                                               cex.main = 0.8),
                                   plot.axes = {graphics::contour(x = gx,
                                                        y = gy,
                                                        z = z,
                                                        nlevels = nlevels,
                                                        drawlabels = FALSE,
                                                        axes = FALSE,
                                                        frame.plot = FALSE,
                                                        add = TRUE);
                                                graphics::axis(1);
                                                graphics::axis(2)},
                                   ...
          )
        } else {
          graphics::filled.contour(x = gx,
                                     y = gy,
                                     z = z,
                                     nlevels = nlevels,
                                     color.palette = function(n)
                                       grDevices::hcl.colors(n, col.pal),
                                     xlab = xlab,
                                     ylab = ylab,
                                     key.title = graphics::title(main = " Contour",
                                                                 cex.main = 0.8),
                                     ...
          )
        }
      } else {
        stop("delta has to take a value greater or equal to zero and smaller than 1/2")
      }
    } else {
      stop("n.grid has to be either of length 1 or 2, higher dimensions are not supported.")
    }
  } else if (n.grid < 2) {
    stop("Please supply a higher grid density (>= 2)")
  } else {
    stop(
      "Please check the number of dimensions of the supplied copula, only the bivariate case is supported"
    )
  }
}
