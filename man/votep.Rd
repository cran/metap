\name{votep}
\alias{votep}
\alias{print.votep}
\title{Combine p-values by the vote counting method}
\description{
Combine \mjseqn{p}-values by the vote counting method\loadmathjax
}
\usage{
votep(p, alpha = 0.5)
\method{print}{votep}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{\sigvec}
  \item{alpha}{A value defining the significance value limits, see Details}
  \item{x}{An object of class \sQuote{\code{votep}}}
  \item{...}{Other arguments to be passed through}
}
\details{By default splits the \mjseqn{p}-values at 0.5 with those below
counting as positive and those above counting as
negative.
However setting alpha
allows specifying a neutral zone.
In that case values between alpha and 1 - alpha will be
assumed to be neutral and only those outside the zone
counted as positive or negative.
If the value of alpha is greater than unity
it is assumed to be a percentage.

\ltlt{two}

\plotmethod
}
\value{An object of class \sQuote{\code{votep}}
and \sQuote{\code{votep}}, a list with entries
  \item{p}{\mjseqn{p} value}
  \item{pos}{Number of positives}
  \item{neg}{Number of negatives}
  \item{alpha}{The cut--off significance value lying in the range 0 to 1}
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
data(dat.metap)
beckerp <- dat.metap$beckerp
votep(beckerp)
}
\keyword{ htest }
