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
#' @param delta double. A value in the range [0, 0.5) for changing the
#' evaluation boundaries by adding it to the lower limits of x and y and
#' subtracting it from the upper limits of x and y. Defaults to zero.
#' @param xlim double. Range of x values to be plotted. Defaults to [0, 1].
#' @param ylim double. Range of y values to be plotted. Defaults to [0, 1].
#' @param xlab character. Title for x-axis.
#' @param ylab character. Title for y-axis.
#' @param main character. Title for the main plot. Set to NULL to omit the title.
#' @param nlevels integer. Number of contour levels to be plotted.
#' @param col.pal character. Name of the color palette to be used for the contour
#' levels, provided by the function hcl.colors() in the grDevices package.
#' @param asp double. Aspect ratio of the axes. Defaults to 1. Use NA to fit
#' the aspect ratio to the available drawing area.
#' @param con.lines logical. Draw contour lines if TRUE (default).
#' @param ... character, function, or integer. Additional graphical parameters.
#' @return A contourplot with the values given by FUN for a copula of the form
#' given by the supplied copula object.
#'
#' @examples
#' \donttest{
#' excop <- claycop(par = 0.5, dim = 2)
#' # Plotting the cdf
#' coptourplot(copula = excop, FUN = "cdf")
#' # Plotting the pdf
#' coptourplot(copula = excop, FUN = "pdf", n.grid = 42, nlevels = 16,
#'             col.pal = "YlGnBu",
#'             plot.title = title(main = "Clayton Copula PDF"))
#' }
#'
#' @export

coptourplot <- function (copula,
                         FUN,
                         n.grid = 26,
                         delta = 0,
                         xlim = 0:1,
                         ylim = 0:1,
                         xlab = quote(u[1]),
                         ylab = quote(u[2]),
                         main = paste(FUN, "of", copula$family, "Copula",
                                      sep = " "),
                         nlevels = 20,
                         col.pal = "Viridis",
                         asp = 1,
                         con.lines = TRUE,
                         ...) {
  if (copula$dimension == 2 & n.grid >= 2) {
    if (length(n.grid) == 1) {
      n.grid <- rep(n.grid, 2)
    }
    if (length(n.grid) == 2) {
      if (0 <= delta && delta < (1 / 2)) {

        # Create xy-grid and calculate corresponding z-values
        coord <- zofgrid(cop = copula, FUN = FUN, n = n.grid, delta = delta,
                         xlim = xlim, ylim = ylim)
        gx <- coord$gx
        gy <- coord$gy
        z <- coord$z
        zlim <- range(z, na.rm = TRUE)
        if (identical(min(zlim, na.rm = TRUE), max(zlim, na.rm = TRUE))) {
          warning("All z values are identical.")
          xlab = "u1"
          ylab = "u2"
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
                                   main = main,
                                   key.title = graphics::title(main = "Levels",
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
                                   asp = asp,
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
                                   main = main,
                                   key.title = graphics::title(main = " Contour",
                                                               cex.main = 0.8),
                                   asp = asp,
                                   ...
          )
        }
      } else {
        stop(
          "delta has to take a value greater or equal to zero and smaller than
          1/2"
          )
      }
    } else {
      stop(
        "n.grid has to be either of length 1 or 2, higher dimensions are not
        supported."
        )
    }
  } else if (n.grid < 2) {
    stop(
      "Please supply a higher grid density (>= 2)"
      )
  } else {
    stop(
      "Please check the number of dimensions of the supplied copula, only the
      bivariate case is supported"
    )
  }
}
