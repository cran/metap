\name{sumz}
\alias{sumz}
\alias{print.sumz}
\title{Combine p-values using the sum of z method}
\description{Combine \eqn{p}-values using the sum z method}
\usage{
sumz(p, weights = NULL, data = NULL, subset = NULL, na.action = na.fail)
\method{print}{sumz}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{A vector of \eqn{p}-values}
  \item{weights}{A vector of weights}
  \item{data}{Optional data frame containing variables}
  \item{subset}{Optional vector of logicals to specify a subset of the \eqn{p}-values}
  \item{na.action}{A function indicating what should happen when data
      contains NAs}
  \item{x}{An object of class \sQuote{\code{sumz}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as \deqn{\frac{\sum (w z(p))}{\sqrt {\sum w ^ 2}}}{sum (w * z(p)) / sqrt(sum (w * w))}
is a \eqn{z} where \eqn{k} is the number of studies
and \eqn{w} are the weights.
By default the weights are equal.
In the absence of effect sizes (in which case a method for
combining effect sizes woud be more appropriate anyway)
best resuts are believed to be obtained with weights
proportional to the square root of the sample sizes
(see Zaykin reference).

The values of \eqn{p} should be such that \eqn{0<p<1}.
A warning is issued if this means that studies are omitted
and an error results if as a result fewer than two studies remain.
If the omitted \eqn{p} values had supplied weights
a further warning is issued.

The plot method for class \sQuote{\code{metap}}
calls \code{\link{schweder}} on the valid
\eqn{p}-values
}
\value{An object of class \sQuote{\code{sumz}} and
\sQuote{\code{metap}}, a list with entries
  \item{z}{Transformed sum of \eqn{z} values}
  \item{p}{Associated \eqn{p}-value}
  \item{validp}{The input vector with illegal values removed}
  \item{weights}{The weight vector corresponding to \code{validp}}
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

Zaykin, D V. Optimally weighted \eqn{Z}-test is a powerful method
for combining probabilities in meta-analysis.
\emph{Journal of Evolutionary Biology}
24:1836-1841, 2011
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
sumz(rosenthal$p) # 2.39, p = 0.009
sumz(p, df, rosenthal) # 3.01, p = 0.0013
data(validity)
sumz(validity) # z = 8.191, p = 1.25 * 10^{-16}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
