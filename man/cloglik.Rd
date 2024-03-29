% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cloglik.R
\name{cloglik}
\alias{cloglik}
\title{Computation of log-Likelihood of Copulas}
\usage{
cloglik(copula, data, parameter = NULL)
}
\arguments{
\item{copula}{A copula object. Decides the copula family for which the
log-Likelihood function is to be calculated.}

\item{data}{The data to base the Likelihood on. Data points have to be normed.
Copula data has to lie within [0, 1]^d.}

\item{parameter}{double. Optional argument. Desired Parameter value in case
deviation from supplied copula object is desired.}
}
\value{
The value of the calculated log-Likelihood function.
}
\description{
Calculates the log-Likelihood function of a chosen Copula family.
}
\examples{
\donttest{
exc <- claycop(par = 5, dim = 2)
U <- matrix(c(0.7026, 0.6359, 0.4116, 0.8833), nrow = 2, byrow = TRUE)
ll <- cloglik(copula = exc, data = U)
}

}
\references{
Hofert et al. (2018). Elements of Copula Modeling with R.
Springer.

Nelsen (2006). An introduction to copulas. Springer Series in Statistics.
Second Edition.
}
\seealso{
\code{\link{cfit}}
}
