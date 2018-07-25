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
  \item{p}{A vector of \eqn{p}-values}
  \item{x}{An object of class \sQuote{\code{meanz}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Let \deqn{\bar{z} = \sum_{i=1}^k \frac{z(p_i)}{k}}
and \deqn{s_{\bar{z}} = \frac{s_z}{\sqrt{k}}}
Defined as
\deqn{%
\frac{\bar{z}}{s_{\bar{z}}} > t_{k-1}(\alpha)
}{((\bar{z} / s_\bar{z}) > t_{k-1}(\alpha)}

The values of \eqn{p} should be such that
\eqn{0\le{}p\le{}1}{0<=p<=1} and a warning is given if this
is not true.
An error is given if possibly as a result of removing
them fewer than two valid \eqn{p} values remain.
As can be seen if all the \eqn{p_i} are equal or close
to equal this gives a \eqn{t=\pm\infty} leading to
a returned value of 0 or 1. 

The plot method for class \sQuote{\code{metap}}
calls \code{\link{schweder}} on the valid
\eqn{p}-values
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


\examples{
data(beckerp)
meanz(beckerp)
}
\keyword{ htest }
