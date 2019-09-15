#' Perspective Plot of Copula
#'
#' Gives a perspective plot of a surface for a given copula object for a specified function
#'
#' @param copula ArCop object. Supplies the copula to be used for the perspective plot.
#' @param FUN function. Gives the function to be plotted.
#' @param n.grid integer.
#' @param delta numeric.
#' @param nlevels integer.
#' @param xlim integer.
#' @param ylim integer.
#' @param xlab character.
#' @param ylab character.
#' @return A perspective plot of a surface over the x-y plane using the values given by FUN for a copula of the form given by the supplied copula object.
#'
#' @examples
#' \donttest{
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exS <- rCop(copula = exC, n = 1000)
#' }
#'
#' @export

coppersplot <- function (copula,
                         FUN,
                         n.grid = 26,
                         delta = 0,
                         xlim = 0:1,
                         ylim = 0:1,
                         zlim = NULL,
                         xlab = "u1",
                         ylab = "u2",
                         theta = -45/2,
                         phi = 30,
                         expand = 0.75,
                         col = c("red", "yellow"),
                         shade = 0.2,
                         ticktype = "detail",
                         zlab = deparse(substitute(FUN))[1],
                         ...) {
  if (copula$dimension == 2 & n.grid >= 2) {
    if (length(n.grid) == 1) {
      n.grid <- rep(n.grid, 2)
    }
    if (length(n.grid) == 2) {
      if (0 <= delta && delta < (1 / 2)) {
        gx <- seq(xlim[1] + delta, xlim[2] - delta, length.out = n.grid[1])
        gy <- seq(ylim[1] + delta, ylim[2] - delta, length.out = n.grid[2])
        theta_ <- copula$parameters
        z <- app(
          x = gx,
          y = gy,
          f = FUN,
          par = theta_
        )
        if (is.null(zlim)) {
          zlim <- range(z, na.rm = TRUE)
          if (min(zlim, na.rm = TRUE) == max(zlim, na.rm = TRUE)) {
            stop("Perspective plots of a horizontal plane are not supported.")
          }
        }
        if (length(col) == 2) {
          colpal <- colorRampPalette(col)
          cols <- colpal(100)
          z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
          z.facet.range <- cut(z.facet.center, 100)
          col <- cols[z.facet.range]
        }
        graphics::persp(
          x = gx,
          y = gy,
          z = z,
          zlim = zlim,
          xlab = xlab,
          ylab = ylab,
          zlab = zlab,
          theta = theta,
          phi = phi,
          expand = expand,
          col = col,
          shade = shade,
          ticktype = ticktype,
          ...
        )
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