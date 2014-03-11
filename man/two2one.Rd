\name{two2one}
\alias{two2one}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert two-sided p-values to one-sided}
\description{Convert two-sided \eqn{p}-values to one-sided}
\usage{
two2one(p, two = NULL, invert = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{A vector of p-values}
  \item{two}{A logical vector defining which
   \eqn{p}-values are two-sided and to be converted}
  \item{invert}{A logical vector defining which
  \eqn{p}-values are to be inverted}
}
\details{The \eqn{p}-values supplied to the other functions should be
one-sided and all in te same direction.
This convenience function will convert two-sided to
one-sided and invert as necessary.
By default it is assumed that all \eqn{p}-values are two-sided
and the function converts them all to one-sided.
Use \code{invert} to specify if some of the tests
resulted in outcomes in the other direction.
So for example a value of 0.05 wil be converted to 0.025 unless
\code{invert} is TRUE in which case it is converted to 0.975.}
\value{A vector of one-sided p-values all in the correct direction
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{Michael Dewey}

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
data(rosenthal)
twop <- with(rosenthal, (pt(t, df)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
