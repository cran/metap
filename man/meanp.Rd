\name{meanp}
\alias{meanp}
\alias{print.meanp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Combine p values by the mean p method}
\description{Combine \mjseqn{p} values by the mean p method\loadmathjax}
\usage{
meanp(p)
\method{print}{meanp}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{\sigvec}
  \item{x}{An object of class \sQuote{\code{meanp}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Defined as
\mjdeqn{z = (0.5 - \bar{p}) \sqrt{12k}}{z = (0.5 - mean(p)) * sqrt(12k)}
which is a standard normal and due to \insertCite{edgington72b;textual}{metap}
\insertNoCite{becker94}{metap}\insertNoCite{rosenthal78}{metap}

\lele{four}

\plotmethod
}
\value{
An object of class \sQuote{\code{meanp}} and \sQuote{\code{metap}},
a list with entries
  \item{z}{Value of \mjseqn{z}}
  \item{p}{Associated \mjseqn{p}-value}
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
data(dat.metap)
rosenthal <- dat.metap$rosenthal
meanp(rosenthal$p) # 2.17, p = 0.015 one tailed
}
\keyword{ htest }
