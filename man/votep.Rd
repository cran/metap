\name{votep}
\alias{votep}
\alias{print.votep}
\title{Combine p-values by the vote counting method}
\description{
Combine \eqn{p}-values by the vote counting method
}
\usage{
votep(p, alpha = c(0.5, 0.5))
\method{print}{votep}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{\sigvec}
  \item{alpha}{A vector of length 2 defining the significance value limits}
  \item{x}{An object of class \sQuote{\code{votep}}}
  \item{...}{Other arguments to be passed through}
}
\details{By default splits the \eqn{p}-values at 0.5 with those below
counting as positive and those above counting as
negative.
However setting alpha
allows for a different choice of cut-off
and also by specifying two different values
for a neutral zone.
If either value of alpha is greater than unity
it is assumed to be a percentage.

\ltlt{two}

\plotmethod
}
\value{An object of class \sQuote{\code{votep}}
and \sQuote{\code{votep}}, a list with entries
  \item{p}{\eqn{p} value}
  \item{pos}{Number of positives}
  \item{neg}{Number of negatives}
  \item{alpha}{A vector of length 2 defining the significance values and
     lying in the range 0 to 1}
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
votep(beckerp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
