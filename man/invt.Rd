\name{invt}
\alias{invt}
\alias{print.invt}
\title{Combine p values using inverse t method}
\description{Combine p values using the inverse t method\loadmathjax}
\usage{
invt(p, k, data = NULL, subset = NULL, na.action = na.fail)
\method{print}{invt}(x, ...)
}
\arguments{
  \item{p}{\sigvec}
  \item{k}{A vector of degrees of freedom to use, see details}
  \item{data}{Optional data frame containing variables}
  \item{subset}{Optional vector of logicals to specify a subset of the \mjseqn{p}-values}
  \item{na.action}{A function indicating what should happen when data
      contains NAs}
  \item{x}{An object of class \sQuote{\code{invt}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as 
\mjdeqn{\frac{\sum_{i=1}^n t_{k_i}(p_i)}%
{\sqrt{\sum_{i=1}^n \frac{k_i}{k_i - 2}}}%
> z(\alpha)%
}{\sum t (p) /%
\sqrt{\sum k / (k - 2)}%
> z(\alpha)}

If \code{k} is a single value it is used for all the \code{p}
of which there are \code{n}.
If any value of \mjseqn{k_i\le2} then the corresponding \mjseqn{p_i}
is not included.

\ltle{two}

\plotmethod
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

\seealso{
See also \code{\link{plotp}}
}

\examples{
data(beckerp)
invt(beckerp, 50)
}
\keyword{ htest }
