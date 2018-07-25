\name{sumlog}
\alias{sumlog}
\alias{Fisher's method}
\alias{chi-square (2) method}
\alias{print.sumlog}
\title{Combine p-values by the sum of logs (Fisher's) method
}
\description{Combine \eqn{p}-values by the sum of logs method,
also known as Fisher's method, and sometimes as the
chi-square (2) method.}
\usage{
sumlog(p)
\method{print}{sumlog}(x, ...)
}
\arguments{
  \item{p}{A vector of \eqn{p}-values}
  \item{x}{An object of class \sQuote{\code{sumlog}}}
  \item{...}{Other arguments to be passed through}
}
\details{
The method relies on the fact that
\deqn{\sum - 2 \log p}{sum -2 log p}
is a chi-squared with \eqn{2 k}{2 * k} df where \eqn{k} is the number
of studies \insertCite{fisher25}{metap}.
\insertNoCite{becker94}{metap}
\insertNoCite{rosenthal78}{metap}
\insertNoCite{sutton00}{metap}

The values of \eqn{p} should be such that
\eqn{0<p\le{}1}{0<p<=1} and a warning is given if that
is not true.
An error is given
if possibly as a result of deletions
fewer than two studies remain.

The plot method for class \sQuote{\code{metap}}
calls \code{\link{schweder}} on the valid
\eqn{p}-values.
Inspection of the distribution of \eqn{p}-values is highly recommended
as extreme values in opposite directions do not cancel out.
See last example.
This may not be what you want.
}
\value{
An object of class \sQuote{\code{sumlog}} and \sQuote{\code{metap}},
a list with entries
  \item{chisq}{Value of chi-squared statistic}
  \item{df}{Associated degrees of freedom}
  \item{p}{Associated p-value}
  \item{validp}{The input vector with the illegal values removed}
%% ...
}
\references{
\insertAllCited{}
}

\author{Michael Dewey}

\seealso{
See also \code{\link{schweder}}
}
\examples{
data(teachexpect)
sumlog(teachexpect) # chisq = 69.473, df = 38, p = 0.0014, from Becker
data(beckerp)
sumlog(beckerp) # chisq = 18.533, df = 10, sig
data(rosenthal)
sumlog(rosenthal$p) # chisq = 22.97, df = 10, p = 0.006 one sided
data(cholest)
sumlog(cholest) # chisq = 58.62, df = 68, p = 0.78
data(validity)
sumlog(validity) # chisq = 159.82, df = 40, p = 2.91 * 10^{-16}
sumlog(c(0.0001, 0.0001, 0.9999, 0.9999)) # is significant
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
