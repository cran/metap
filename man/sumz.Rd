\name{sumz}
\alias{sumz}
\alias{Stouffer's method}
\alias{Sum of normals method}
\alias{print.sumz}
\title{Combine p-values using the sum of z (Stouffer's) method}
\description{Combine \mjseqn{p}-values using the sum z method\loadmathjax}
\usage{
sumz(p, weights = NULL, data = NULL, subset = NULL, na.action = na.fail)
\method{print}{sumz}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{\sigvec}
  \item{weights}{A vector of weights}
  \item{data}{Optional data frame containing variables}
  \item{subset}{Optional vector of logicals to specify
     a subset of the \mjseqn{p}-values}
  \item{na.action}{A function indicating what should happen when data
      contains NAs}
  \item{x}{An object of class \sQuote{\code{sumz}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as \mjdeqn{\frac{\sum_{i=1}^k  w_i z(p_i)}{\sqrt {\sum_{i=1}^k  w_i ^ 2}}}{sum (w * z(p)) / sqrt(sum (w * w))}
is a \mjseqn{z} where \mjseqn{k} is the number of studies
and \mjseqn{w} are the weights \insertCite{stouffer49}{metap}.
By default the weights are equal.
In the absence of effect sizes (in which case a method for
combining effect sizes would be more appropriate anyway)
best results are believed to be obtained with weights
proportional to the square root of the sample sizes
\insertCite{zaykin11}{metap}

\insertNoCite{becker94}{metap}
\insertNoCite{rosenthal78}{metap}

\ltlt{two} If the omitted \mjseqn{p} values had supplied weights
a further warning is issued.

\plotmethod
}
\value{An object of class \sQuote{\code{sumz}} and
\sQuote{\code{metap}}, a list with entries
  \item{z}{Transformed sum of \mjseqn{z} values}
  \item{p}{Associated \mjseqn{p}-value}
  \item{validp}{The input vector with illegal values removed}
  \item{weights}{The weight vector corresponding to \code{validp}}
}
\references{
\insertAllCited{}
}
\author{Michael Dewey}
\seealso{
See also \code{\link{plotp}}
}
\examples{
data(teachexpect)
sumz(teachexpect) # z = 2.435, p = 0.0074, from Becker
data(beckerp)
sumz(beckerp) # z = 1.53, NS, from Beckerp
data(rosenthal)
sumz(rosenthal$p) # 2.39, p = 0.009
sumz(p, df, rosenthal) # 3.01, p = 0.0013
data(validity)
sumz(validity) # z = 8.191, p = 1.25 * 10^{-16}
}
\keyword{ htest }
