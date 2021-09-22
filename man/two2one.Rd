\name{two2one}
\alias{two2one}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert two-sided p-values to one-sided}
\description{Convert two-sided p-values to one-sided\loadmathjax}
\usage{
two2one(p, two = NULL, invert = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{A vector of p-values}
  \item{two}{A logical vector defining which
   \mjseqn{p}-values are two-sided and to be converted}
  \item{invert}{A logical vector defining which
  \mjseqn{p}-values are to be inverted}
}
\details{The \mjseqn{p}-values supplied to the other functions should be
one-sided and all in the same direction.
This convenience function will convert two-sided to
one-sided and invert as necessary.
By default it is assumed that all \mjseqn{p}-values are two-sided
and the function converts them all to one-sided.
Use \code{invert} to specify if some of the tests
resulted in outcomes in the other direction.
So for example a value of 0.05 will be converted to 0.025 unless
\code{invert} is TRUE in which case it is converted to 0.975.}
\value{A vector of one-sided \mjseqn{p}-values all in the correct direction
}
\author{Michael Dewey}

\examples{
data(dat.metap)
rosenthal <- dat.metap$rosenthal
twop <- with(rosenthal, (pt(t, df)))
two2one(twop, two = rep(FALSE, 5), invert = rep(TRUE, 5)) # restore to one-sided
}
\keyword{ manip }
