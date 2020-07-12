\name{sump}
\alias{sump}
\alias{Edgington's method}
\alias{print.sump}
\title{Combine p-values using the sum of p (Edgington's) method}
\description{Combine \mjseqn{p}-values using the sum p method\loadmathjax}
\usage{
sump(p)
\method{print}{sump}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{\sigvec}
  \item{x}{An object of class \sQuote{\code{sump}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as
\mjdeqn{\frac{(\sum_{i=1}^k p_i)^k}{k!}
- {k \choose 1}\frac{(\sum_{i=1}^k p_i - 1)^k}{k!}
+ {k \choose 2}\frac{(\sum_{i=1}^k  p_i - 2)^k}{k!} ...}{%
((sum p) ^ k) / k! - (k-1)C(1) ((sum p - 1) ^ k) / k! + (k-2)C(2) ((sum p - 2) ^ k) / k! ...}
where there are \mjseqn{k} studies and the series continues
until the numerator
becomes negative \insertCite{edgington72a}{metap}.

Some authors use a simpler version
\mjdeqn{\frac{(\sum_{i=1}^k  p_i)^k}{k!}}{((sum p) ^ k) / k!}
but this can be very conservative when
\mjeqn{\sum_{i=1}^k  p_i > 1}{sum p > 1}.
There seems no particular need to use this method but
it is returned as the value of \code{conservativep}
for use in checking published values.

\lele{two} A warning is given when the internal calculations
are likely to have been subject to numerical error
and an alternative method should be used to check
the result.

\plotmethod
}
\value{An object of class \sQuote{\code{sump}} and
\sQuote{\code{metap}}, a list with entries
  \item{p}{The transformed sum of the \mjseqn{p}-values}
  \item{conservativep}{See details}
  \item{validp}{The input vector with illegal values removed}
}
\references{

\insertAllCited{}

}
\author{Michael Dewey}
\seealso{
See also \code{\link{plotp}}
}
\examples{
data(edgington)
sump(edgington) # p = 0.097
}
\keyword{ htest }
