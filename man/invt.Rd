\name{invt}
\alias{invt}
\alias{print.invt}
\title{Combine p values using inverse t method}
\description{Combine p values using the inverse t method}
\usage{
invt(p, k, data = NULL, subset = NULL, na.action = na.fail)
\method{print}{invt}(x, ...)
}
\arguments{
  \item{p}{A vector of \eqn{p}-values}
  \item{k}{A vector of degrees of freedom to use, see details}
  \item{data}{Optional data frame containing variables}
  \item{subset}{Optional vector of logicals to specify a subset of the \eqn{p}-values}
  \item{na.action}{A function indicating what should happen when data
      contains NAs}
  \item{x}{An object of class \sQuote{\code{invt}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as 
\deqn{\frac{\sum_{i=1}^n t_{k_i}(p_i)}%
{\sqrt{\sum_{i=1}^n \frac{k_i}{k_i - 2}}}%
> z(\alpha)%
}{\sum_{i=1}^n t_{k_i}(p_i) /%
\sqrt{\sum_{i=1}^n {k_i} / {k_i - 2}}%
> z(\alpha}

If \code{k} is a single value it is used for all the \code{p}
of which there are \code{n}.
If any value of \eqn{k<1} then the corresponding \code{p}
is not included.

The values of \eqn{p} should be such that
\eqn{0<p\le{}1}{0<p<=1} and a warning is given if that
is not true.
An error is given
if possibly as a result of deletions
fewer than two studies remain.

The plot method for class \sQuote{\code{metap}}
calls \code{\link{schweder}} on the valid
\eqn{p}-values.
}
\value{
An object of class \sQuote{\code{invt}} and \sQuote{\code{metap}},
a list with entries
  \item{z}{Value of z}
  \item{p}{Associated p-value}
  \item{validp}{The input vector with the illegal values removed}
}
\references{
\insertRef{becker94}{metap}
}
\author{Michael Dewey}

\examples{
data(beckerp)
invt(beckerp, 50)
}
\keyword{ htest }
