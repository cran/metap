\name{meanz}
\alias{meanz}
\alias{print.meanz}
\title{Combine p values using mean z method}
\description{Combines p values using the mean of z
method\loadmathjax}
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
Let \mjdeqn{\bar{z} = \sum_{i=1}^k \frac{z(p_i)}{k}}{bar{z} = sum(z(p) / k)}
and \mjdeqn{s_{\bar{z}} = \frac{s_z}{\sqrt{k}}}{s_{bar{z}} = s_z / sqrt k }
Defined as
\mjdeqn{%
\frac{\bar{z}}{s_{\bar{z}}} > t_{k-1}(\alpha)
}{((bar{z} / s_bar{z}) > t_{k-1}(alpha)}

\lele{two}
As can be seen if all the \mjseqn{p_i} are equal or close
to equal this gives a \mjeqn{t=\pm\infty}{t=+-infty} leading to
a returned value of 0 or 1. 
A set of \mjseqn{p} values with small variance will
necessarily give a small \mjseqn{p} value which may be smaller
than that for another set all of whose primary values are less
than any in the first set.
See examples for a demonstration.

\plotmethod
}
\value{An object of class \sQuote{\code{meanz}} and
\sQuote{\code{metap}}, a list with entries
  \item{z}{The value of the mean \mjseqn{z} statistic}
  \item{p}{The associated \mjseqn{p} value}
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
