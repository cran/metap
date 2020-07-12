\name{invchisq}
\alias{invchisq}
\alias{Lancaster's method}
\alias{print.invchisq}
\title{Combine p values using inverse chi squared method}
\description{Combine \mjseqn{p}-values by the
inverse chi-squared method,
also known as Lancaster's method\loadmathjax
}
\usage{
invchisq(p, k, data = NULL, subset = NULL, na.action = na.fail)
\method{print}{invchisq}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{\sigvec}
  \item{k}{A vector of degrees of freedom to use, see details}
  \item{data}{Optional data frame containing variables}
  \item{subset}{Optional vector of logicals to specify a subset of the \mjseqn{p}-values}
  \item{na.action}{A function indicating what should happen when data
      contains NAs}
  \item{x}{An object of class \sQuote{\code{invchisq}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as
\mjdeqn{\sum_{i=1}^n \chi^2_{k_i}(p_i) > \chi^2_{\sum{k_i}}(\alpha)}{\sum \chi^2_k(p) > \chi^2_\sum{k}(\alpha)}

If \code{k} is a single value it is used for all the \code{p}
of which there are \code{n}.
If any value of \mjseqn{k_i\le0} then the corresponding \mjseqn{p_i}
is not included.

\ltle{two}

\plotmethod
\nocancel
}
\value{
An object of class \sQuote{\code{invchisq}} and \sQuote{\code{metap}},
a list with entries
  \item{chisq}{Value of chi-squared statistic}
  \item{df}{Associated degrees of freedom}
  \item{p}{Associated p-value}
  \item{validp}{The input vector with the illegal values removed}
}
\references{
\insertRef{becker94}{metap}

\insertRef{lancaster49}{metap}
}
\author{
Michael Dewey
}
\seealso{
See also \code{\link{sumlog}} and \code{\link{plotp}}
}
\examples{
data(beckerp)
invchisq(beckerp, 2) # same as sumlog
invchisq(c(0.999, 0.999, 0.001, 0.001), 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
