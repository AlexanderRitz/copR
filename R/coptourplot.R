#' Contourplot of Copula
#'
#' Gives a contourplot of a given copula object for a specified Function
#'
#' @param copula A copula object. Supplies the copula to be used for the
#' contourplot.
#' @param FUN character. Decides whether the cdf or pdf of the supplied copula
#' is to be plotted. Accepts "cdf" or "pdf" as input.
#' @param n.grid integer.
#' @param delta numeric.
#' @param nlevels integer.
#' @param xlim integer.
#' @param ylim integer.
#' @param xlab character.
#' @param ylab character.
#' @return A contourplot with the values given by FUN for a copula of the form
#' given by the supplied copula object.
#'
#' @examples
#' \donttest{
#' exCop <- clayCop(par = 5, dim = 2)
#' #Plotting the cdf
#' coPtourplot(copula = exCop, FUN = "cdf", n.grid = 26, delta = 0, nlevels = 20,
#' xlim = 0:1, ylim = 0:1, xlab = quote(u[1]), ylab = quote(u[2]))
#' #Plotting the pdf
#' coPtourplot(copula = exCop, FUN = "pdf", n.grid = 26, delta = 0, nlevels = 20,
#' xlim = 0:1, ylim = 0:1, xlab = quote(u[1]), ylab = quote(u[2]))
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
  ylab = quote(u[2])) {
  if (copula$dimension == 2 & n.grid >= 2) {
    if (length(n.grid) == 1) {
      n.grid <- rep(n.grid, 2)
    }
    if (length(n.grid) == 2) {
      if (0 <= delta && delta < (1 / 2)) {
        gx <- seq(xlim[1] + delta, xlim[2] - delta, length.out = n.grid[1])
        gy <- seq(ylim[1] + delta, ylim[2] - delta, length.out = n.grid[2])
        theta <- copula$parameter
        if (FUN == "cdf") {
          value <- app(x = gx, y = gy, f = copula$distribution$cdf, par = theta)
        } else if (FUN == "pdf") {
          value <- app(x = gx, y = gy, f = copula$distribution$pdf, par = theta)
        } else {
          stop("Please choose between plotting either the cdf or pdf of the copula.")
        }
        graphics::filled.contour(
          x = gx,
          y = gy,
          z = value,
          nlevels = nlevels,
          color = function(n)
            grDevices::hcl.colors(n, "RedOr"),
          xlab = xlab,
          ylab = ylab,
          key.title = graphics::title(main = " Contour", cex.main = 0.8)
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
    stop("Please check the number of dimensions of the supplied copula, only the bivariate case is supported")
  }
}
