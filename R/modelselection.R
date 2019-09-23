#' Model selection criteria for fitcop objects: AIC
#'
#' Calculates AIC for a supplied fitted copula object.
#'
#' @param copula A list of class "fitcop". Supplies the value of the
#' log-likelihood function.
#' @return The value of the AIC for the supplied copula and the related maximum
#' likelihood estimation.
#'
#' @examples
#' \donttest{
#' exc <- claycop(par = 5, dim = 2)
#' U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
#' byrow = TRUE)
#' fit <- cfit(copula = exc, data = U)
#' c_aic(fit)
#' }
#'
#' @export
#' @seealso \code{\link{cloglik}} and \code{\link{c_bic}}

c_aic <- function(copula) {
  if (is.fitcop(copula)) {
    AIC <- -2 * copula$loglikelihood + 2
    return(AIC)
  } else {
    stop("Please supply a fitted copula, see ?cfit")
  }
}


#' Model selection criteria for fitcop objects: BIC
#'
#' Calculates BIC for a supplied fitted copula object.
#'
#' @param copula A list of class "fitcop". Supplies the value of the
#' log-likelihood function.
#' @return The value of the BIC for the supplied copula and the related maximum
#' likelihood estimation.
#'
#' @examples
#' \donttest{
#' exc <- claycop(par = 5, dim = 2)
#' U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833, 0.3127, 0.4035), nrow = 3,
#' byrow = TRUE)
#' fit <- cfit(copula = exc, data = U)
#' c_bic(fit)
#' }
#'
#' @export
#' @seealso \code{\link{cloglik}} and \code{\link{c_aic}}

c_bic <- function(copula) {
  if (is.fitcop(copula)) {
    BIC <- -2 * copula$loglikelihood + log(copula$observations)
    return(BIC)
  } else {
    stop("Please supply a fitted copula, see ?cfit")
  }
}
