\name{meanp}
\alias{meanp}
\alias{print.meanp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Combine p values by the mean p method}
\description{Combine \eqn{p} values by the mean p method}
\usage{
meanp(p)
\method{print}{meanp}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{A vector of \eqn{p}-values}
  \item{x}{An object of class \sQuote{\code{meanp}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as \deqn{z = (0.5 - \bar{p}) \sqrt{12k}}{z = (0.5 - mean(p)) * sqrt(12k)}
which is a standard normal

The values of \eqn{p} should be such that \eqn{0\le{}p\le{}1}{0<=p<=1}.
A warning is issued if this means that studies are omitted
and an error results if,
possibly after deletion of illegal values, fewer than four studies remain.

The plot method for class \sQuote{\code{metap}} calls \code{\link{schweder}} on the valid
\eqn{p}-values
}
\value{
An object of class \sQuote{\code{meanp}} and \sQuote{\code{metap}},
a list with entries
  \item{z}{Value of \eqn{z}}
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
data(rosenthal)
meanp(rosenthal$p) # 2.17, p = 0.015 one tailed
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
