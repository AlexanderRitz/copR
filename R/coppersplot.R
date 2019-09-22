#' Perspective Plot of Copula
#'
#' Gives a perspective plot of the surface of a given copula object for a
#' specified function.
#'
#' @param copula A copula object. Supplies the copula to be used for the
#' perspective plot.
#' @param FUN character. Decides whether the cdf or pdf of the supplied copula
#' is to be plotted. Accepts "cdf" or "pdf" as input.
#' @param n.grid integer. Number of grid points in x- and y-direction. Vector of
#' length two or one (used for both directions). Determines the number of facets.
#' @param delta double. A value in the range [0, 0.5) for changing the
#' evaluation boundaries by adding it to the lower limits of x and y and
#' subtracting it from the upper limits of x and y. Defaults to zero.
#' @param xlim double. Range of x values to be plotted. Defaults to [0, 1].
#' @param ylim double. Range of y values to be plotted. Defaults to [0, 1].
#' @param xlab character. Title for x-axis.
#' @param ylab character. Title for y-axis.
#' @param zlab character. Title for z-axis.
#' @param main character. Main title for the plot. Set to NULL to omit the title.
#' @param theta double. Azimuthal viewing direction.
#' @param phi double. Viewing colatitude.
#' @param expand double. Expansion factor applied to the z coordinates.
#' @param col.pal character. Name of the color palette to be used for the sur-
#' face facets, provided by the function hcl.colors() in the grDevices package.
#' @param border character, function, or integer. Color of the line drawn around
#' the surface facets. The default NA shows no borders. NULL corresponds to
#' par("fg").
#' @param shade double. Shade of surface facets.
#' @param ticktype character. "simple" draws just an arrow, "detailed" draws
#' normal ticks as in 2D plots.
#' @param ... character, function, or integer. Additional graphical parameters.
#'
#' @return A perspective plot of a surface over the x-y plane using the values
#' given by FUN for a copula of the form given by the supplied copula object.
#'
#' @examples
#' \donttest{
#' excop <- claycop(par = 1, dim = 2)
#' # Plotting the cdf
#' coppersplot(copula = excop, FUN = "cdf")
#' # Plotting the pdf
#' coppersplot(copula = excop, FUN = "pdf", n.grid = 20, delta= 0.1,
#'             theta = -30, col.pal = "Plasma", border = NULL,
#'             main = "Clayton Copula PDF", cex.main = 0.8)
#' }
#'
#' @export

coppersplot <- function (copula,
                         FUN,
                         n.grid = 50,
                         delta = 0,
                         xlim = 0:1,
                         ylim = 0:1,
                         xlab = "u1",
                         ylab = "u2",
                         zlab = FUN,
                         main = paste(FUN, "of", copula$family, "Copula",
                                      sep = " "),
                         theta = -45/2,
                         phi = 30,
                         expand = 0.75,
                         col.pal = "Viridis",
                         border = NA,
                         shade = NA,
                         ticktype = "detail",
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
          zlim <- 0:max(z, na.rm = TRUE)
        }

        # Calculate colors of facets corresponding to z-values at facet centres
        nrz <- nrow(z)
        ncz <- ncol(z)
        ncol <- min(n.grid) - 1
        color <- grDevices::hcl.colors(ncol, col.pal)
        zfacet <- (z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]) / 4
        facetcol <- cut(zfacet, ncol)

        # Draw the perspective plot
        graphics::persp(x = gx,
                        y = gy,
                        z = z,
                        zlim = zlim,
                        xlab = xlab,
                        ylab = ylab,
                        zlab = zlab,
                        main = main,
                        theta = theta,
                        phi = phi,
                        expand = expand,
                        col = color[facetcol],
                        border = border,
                        shade = shade,
                        ticktype = ticktype,
                        ...
        )
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
