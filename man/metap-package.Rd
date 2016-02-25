\name{metap-package}
\alias{metap-package}
\alias{metap}
\docType{package}
\title{Meta-analysis of p-values}
\description{Combine \eqn{p}-values using a variety of methods}
\details{
\tabular{ll}{
Package: \tab metap\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-10-03\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
Provides a number of ways in which significance levels may be combined
in a meta-analysis.
It includes a number of datasets taken from the literature. 
It also provdes a display and
an informal graphical test due to Schweder and Spjotvol
and the lowest slope line of Benjamini and
Hochberg.
}
\author{Michael Dewey <lists@dewey.myzen.co.uk>}
\references{
Becker, B J. Combining significance levels. In
Cooper, H and Hedges, L V, editors
\emph{A handbook of research synthesis},
chapter 15, pages 215--230.
Russell Sage,
New York, 1994.

Benjamini, Y and Hochberg, Y.
On the adaptive control of the false disovery rate in
multiple testing with independent statistics.
\emph{Journal of Educational and Behavioral Statistics},
25: 60--83, 2000.

Rosenthal, R. Combining the results of independent studies.
\emph{Psychological Bulletin},
85:185--193, 1978.

   Schweder, T and Spjotvoll, E.
   Plots of \eqn{P}--values to evaluate many tests simultaneously.
  \emph{Biometrika}, 69:493--502, 1982.
}
\keyword{ package }
\seealso{
The issue of meta-analysis of signficance levels is not completely
unconnected with the topic of adjustment for multiple
comparisons as in for example \code{\link{p.adjust}}
}
