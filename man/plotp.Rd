\name{plotp}
\alias{plotp}
\title{Q-Q plot of p-values}
\description{
Produces a Q--Q plot of the p-values supplied\loadmathjax
}
\usage{
plotp(pvals, plotversion = "qqconf", ...)
}
\arguments{
  \item{pvals}{A vector of \mjseqn{p}--values}
  \item{plotversion}{Whether to use the qqconf version or the original}
  \item{\dots}{Other parameters to pass through to \code{qqplot}
  and \code{qqline}
}
}
\details{After eliminating out of range \mjseqn{p}--values
and missing values produces a Q--Q plot.
If the package \code{qqconf} is unavailable or
\code{plotversion} is set other than "qqconf"
then the plot uses the function from base graphics.
The line shown is
through the quantiles (by default the 0.25 and 0.75).

If the package \code{qqconf} is available then the plot
uses that package's function and as well as the Q--Q plot
produces a confidence region.
Parameters can be passed to \code{qq_conf_plot} to
exert further control over the plot.

If \code{plotversion} is set to "qqconf"
but package \code{qqconf} is not available
then a warning is given and no plot is produced.

}
\value{
Invisibly returns a list consisting of
  \item{validp}{The valid \mjseqn{p}--values}
}
\author{Michael Dewey}
\seealso{
\code{\link{qqplot}} and \code{\link{qqline}} for
the original plot and \code{\link[qqconf]{qq_conf_plot}} for
the new version.
}
\examples{
data(dat.metap)
cholest <- dat.metap$cholest$p
plotp(cholest)
}
\keyword{hplot}
