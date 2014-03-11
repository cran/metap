\name{logitp}
\alias{logitp}
\alias{print.logitp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Combine p values using logit method}
\description{
Combine \eqn{p} values using logit method
}
\usage{
logitp(p)
\method{print}{logitp}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{A vector of \eqn{p} values}
  \item{x}{An object of class \sQuote{\code{logitp}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as \deqn{t = - \frac{\sum \log\frac{p}{1 - p}}{C}}{%
t = - sum log(p / (1 - p)) / C}
where
\deqn{C = \sqrt\frac{k \pi^2 (5 k + 2)}{3(5 k + 4)}}{C = sqrt((k pi^2 (5 k + 2)) / (3(5 k + 4)))}
and \eqn{k} is the number of studies.

The values of \eqn{p} should be such that \eqn{0<p<1}.
A warning is issued if this means that studies are omitted
and an error results if as a result fewer than two studies remain.

The plot method for class \sQuote{\code{metap}} calls \code{\link{schweder}} on the valid
\eqn{p}-values
}
\value{
An object of class \sQuote{\code{logitp}} and
      \sQuote{\code{metap}}, a list with entries
  \item{t}{Value of Student's \eqn{t}}
  \item{df}{Associated degrees of freedom}
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
}
\author{Michael Dewey}
\seealso{
See also \code{\link{schweder}}
}
\examples{
data(teachexpect)
logitp(teachexpect) # t = 2.763, df = 99, p = 0.0034, from Becker
data(beckerp)
logitp(beckerp) # t = 1.62, df = 29, NS, from Becker
data(validity)
logitp(validity) # t = 9.521, df = 104, p = 3.89 * 10^{-16}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
