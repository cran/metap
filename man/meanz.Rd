\name{meanz}
\alias{meanz}
\alias{print.meanz}
\title{Combine p values using mean z method}
\description{Combines p values using the mean of z
method}
\usage{
meanz(p)
\method{print}{meanz}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{\sigvec}
  \item{x}{An object of class \sQuote{\code{meanz}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Let \deqn{\bar{z} = \sum_{i=1}^k \frac{z(p_i)}{k}}{\bar{z} = \sum(z(p) / k)}
and \deqn{s_{\bar{z}} = \frac{s_z}{\sqrt{k}}}{s_{\bar{z}} = s_z / \sqrt k }
Defined as
\deqn{%
\frac{\bar{z}}{s_{\bar{z}}} > t_{k-1}(\alpha)
}{((\bar{z} / s_\bar{z}) > t_{k-1}(\alpha)}

\lele{two}
As can be seen if all the \eqn{p_i} are equal or close
to equal this gives a \eqn{t=\pm\infty} leading to
a returned value of 0 or 1. 
A set of \eqn{p} values with small variance will
necessarily give a small \eqn{p} value which may be smaller
than that for another set all of whose primary values are less
than any in the first set.
See examples for a demonstration.

\plotmethod
}
\value{An object of class \sQuote{\code{meanz}} and
\sQuote{\code{metap}}, a list with entries
  \item{z}{The value of the mean \eqn{z} statistic}
  \item{p}{The associated \eqn{p} value}
  \item{validp}{The input vector with illegal values removed}
}
\references{
\insertRef{becker94}{metap}
}
\author{Michael Dewey}

\seealso{
See also \code{\link{plotp}}
}

\examples{
data(beckerp)
meanz(beckerp)
meanz(c(0.1, 0.2))  # greater than next example
meanz(c(0.3, 0.31)) # less than above
}
\keyword{ htest }
