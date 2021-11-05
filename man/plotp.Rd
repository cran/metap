\name{plotp}
\alias{plotp}
\title{Q-Q plot of p-values}
\description{
Produces a Q--Q plot of the p-values supplied\loadmathjax
}
\usage{
plotp(pvals, ...)
}
\arguments{
  \item{pvals}{A vector of \mjseqn{p}--values
}
  \item{\dots}{Other parameters to pass through to \code{qqplot}
  and \code{qqline}
}
}
\details{After eliminating out of range \mjseqn{p}--values
and missing values produces a Q--Q plot and the line
through the quantiles (by default the 0.25 and 0.75).
}
\value{
Invisibly returns a list conisting of
  \item{validp}{The valid \mjseqn{p}--values}
}
\author{Michael Dewey}
\seealso{
\code{\link{qqplot}} and \code{\link{qqline}}
}
\examples{
data(dat.metap)
cholest <- dat.metap$cholest$p
plotp(cholest)
}
\keyword{hplot}
