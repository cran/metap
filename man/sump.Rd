\name{sump}
\alias{sump}
\alias{Edgington's method}
\alias{print.sump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Combine p-values using the sum of p (Edgington's) method}
\description{Combine \eqn{p}-values using the sum p method}
\usage{
sump(p)
\method{print}{sump}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{A vector of \eqn{p}-values}
  \item{x}{An object of class \sQuote{\code{sump}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as
\deqn{\frac{(\sum p)^k}{k!}
- {k - 1 \choose 1}\frac{(\sum p - 1)^k}{k!}
+ {k - 2 \choose 2}\frac{(\sum p - 2)^k}{k!}}{%
((sum p) ^ k) / k! - (k-1)C1 ((sum p - 1) ^ k) / k! + (k-2)C2 ((sum p - 2) ^ k) / k!}
where there are \eqn{k} studies and the series continues until the numerator
becomes negative\insertCite{edgington72a}{metap}.

Some authors use a simpler version
\eqn{\frac{(\sum p)^k}{k!}}{((sum p) ^ k) / k!}
where there are \eqn{k} studies
but this can be very conservative when
\eqn{\sum p > 1}{sum p > 1}.
There seems no particular need to use this method but
it is returned as the value of \code{conservativep}
for use in checking published values.

The values of \eqn{p} should be such that
\eqn{0\le{}p\le{}1}{0<=p<=1} and a warning is given if this
is not true.
An error is given if possibly as a result of removing
them fewer than two valid \eqn{p} values remain.
A warning is given when the internal calculations
are likely to have been subject to numerical error
and an alternative method should be used to check
the result.

The plot method for class \sQuote{\code{metap}}
calls \code{\link{schweder}} on the valid
\eqn{p}-values
}
\value{An object of class \sQuote{\code{sump}} and
\sQuote{\code{metap}}, a list with entries
  \item{p}{The transformed sum of the \eqn{p}-values}
  \item{conservativep}{See details}
  \item{validp}{The input vector with illegal values removed}
}
\references{

\insertAllCited{}

}
\author{Michael Dewey}
\seealso{
See also \code{\link{schweder}}
}
\examples{
data(edgington)
sump(edgington) # p = 0.097
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
