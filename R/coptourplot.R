#' Contourplot of Copula
#'
#' Gives a contourplot of a given copula object for a specified Function
#'
#' @param copula ArCop object. Supplies the copula to be used for the contourplot.
#' @param FUN function. Gives the function to be plotted
#' @param n.grid integer.
#' @param delta numeric.
#' @param xlim integer.
#' @param ylim integer.
#' @param xlab character.
#' @param ylab character.
#' @return A contourplot with the vales given by FUN for a copula of the form given by the supplied copula object.
#'
#' @examples
#' \donttest{
#' exC <- copu(Type = "Clayton", par = 5, dim = 2)
#' exS <- rCop(copula = exC, n = 1000)
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
  ylab = quote(u[2])) {
  if (copula$dimension == 2 & n.grid >= 2) {
    if (length(n.grid) == 1) {
      n.grid <- rep(n.grid, 2)
    }
    if (length(n.grid) == 2) {
      if (0 <= delta & delta < (1 / 2)) {
        gx <- seq(xlim[1] + delta, xlim[2] - delta, length.out = n.grid[1])
        gy <- seq(ylim[1] + delta, ylim[2] - delta, length.out = n.grid[2])
        theta <- copula$parameters
        value <- app(x = gx, y = gy, f = FUN, par = theta)
        graphics::filled.contour(
          x = gx,
          y = gy,
          z = value,
          color = function(n)
            grDevices::hcl.colors(n, "RedOr"),
          xlab = xlab,
          ylab = ylab,
          key.title = graphics::title(main = " Contour", cex.main = 0.8)
        )
      }
    }
  }
}
