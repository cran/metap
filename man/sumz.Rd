\name{sumz}
\alias{sumz}
\alias{print.sumz}
\title{Combine p-values using the sum of z method}
\description{Combine \eqn{p}-values using the sum z method}
\usage{
sumz(p)
\method{print}{sumz}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{A vector of \eqn{p}-values}
  \item{x}{An object of class \sQuote{\code{sumz}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as \deqn{\frac{\sum z(p)}{\sqrt k}}{sum z(p) / sqrt(k)}
is a \eqn{z} where \eqn{k} is the number of studies.

The values of \eqn{p} should be such that \eqn{0<p<1}.
A warning is issued if this means that studies are omitted
and an error results if as a result fewer than two studies remain.

The plot method for class \sQuote{\code{metap}}
calls \code{\link{schweder}} on the valid
\eqn{p}-values
}
\value{An object of class \sQuote{\code{sumz}} and
\sQuote{\code{metap}}, a list with entries
  \item{z}{Transformed sum of \eqn{z} values}
  \item{p}{Associated \eqn{p}-value}
  \item{validp}{The input vector with illegal values removed}
}
\references{
Becker, B J. Combining significance levels. In
Cooper, H and Hedges, L V, editors
\emph{A handbook of research synthesis},
chapter 15, pages 215--230.
Russell Sage,
New York, 1994.

Rosenthal, R. Combining the results of independent studies.
\emph{Psychological Bulletin},
85:185--193, 1978.

}
\author{Michael Dewey}
\seealso{
See also \code{\link{schweder}}
}
\examples{
data(teachexpect)
sumz(teachexpect) # z = 2.435, p = 0.0074, from Becker
data(beckerp)
sumz(beckerp) # z = 1.53, NS, from Beckerp
data(rosenthal)
sumz(rosenthal$p) # 2.39, p = 0.009 one sided
data(validity)
sumz(validity) # z = 8.191, p = 1.25 * 10^{-16}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
