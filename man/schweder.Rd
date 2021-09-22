\name{schweder}
\alias{schweder}
\title{Schweder and Spjotvoll plot}
\description{Produces the plot suggested by Schweder and Spjotvoll to display
a collection of \mjseqn{p}-values and also optionally draws the
lowest slope line suggested by Benjamini and Hochberg\loadmathjax
}
\usage{
schweder(p, xlab = "Rank of p", ylab = "p", drawline = NULL,
   bh.lwd = 1, bh.lty = "solid", bh.col = "black",
   ls.control = list(frac = NULL),
   ls.lwd = 1, ls.lty = "dotted", ls.col = "black",
   ab.control = list(a = NULL, b = NULL),
   ab.lwd = 1, ab.lty = "dashed", ab.col = "black", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{A vector of \mjseqn{p}-values}
  \item{xlab}{Label for plot \mjseqn{x}-axis}
  \item{ylab}{Label for plot \mjseqn{y}-axis}
  \item{drawline}{Which line, if any, to draw. See Details}
  \item{bh.lwd}{Width of the lowest slope line}
  \item{bh.lty}{Line type of the lowest slope line}
  \item{bh.col}{Colour of the lowest slope line}
  \item{ls.control}{A list containing a named element \code{frac}. See Details}
  \item{ls.lwd}{Width of the least squares line}
  \item{ls.lty}{Line type of the least squares line}
  \item{ls.col}{Colour of the least squares line}
  \item{ab.control}{A list containing two elements \code{a} and \code{b}. See Details}
  \item{ab.lwd}{Width of the specified line}
  \item{ab.lty}{Line type of the specified line}
  \item{ab.col}{Colour of the specified line}
  \item{\dots}{Further parameters to be passed through to plot}
}
\details{After removing invalid \mjseqn{p}-values plots them.
Any graphics parameters passed in \code{...} affect this part
of the plot only.
Note that the axes are chosen according to the scheme of Banjamini and
Hochberg, Schweder and Spjotvoll used a different system.

By setting \code{drawline} appropriately up to three
lines may be drawn.

If \code{drawline} includes the string \code{"bh"}
the lowest slope line of Benjaimin and Hochberg is drawn.
No further parameters are needed here but the characteristics of the
line may be set: width, line type, and colour.

If \code{drawline} includes the string \code{"ls"}
a least squares line is drawn passing through the point
\mjseqn{k+1, 1}.
The parameter \code{frac} specified what fraction of the
values be used for this and may need experimentation
to obtain a suitable line.
The characteristics of the
line may be set: width, line type, and colour.

If \code{drawline} includes the string \code{"ab"}
a user specified line is drawn.
The parameters \code{a} and \code{b} specify the intercept
and slope. 
The characteristics of the
line may be set: width, line type, and colour.
}
\value{
A list containing
  \item{p}{The values of \mjseqn{p} plotted}
  \item{bh.params}{A list containing at least items \code{a} and \code{b}
      the intercept and slope of the line drawn. This is only included if the
      lowest slope line was drawn}
  \item{ls.params}{A list containing at least items \code{a} and \code{b}
      the intercept and slope of the line drawn. This is only included if the
      least squares line was drawn}
  \item{ab.params}{A list containing at least items \code{a} and \code{b}
      the intercept and slope of the line drawn. This is only included if the
      user specified line was drawn}
}
\references{
\insertRef{becker94}{metap}

\insertRef{benjamini00}{metap}

\insertRef{schweder82}{metap}
}
\author{Michael Dewey}

\examples{
   data(dat.metap)
   teachexpect <- dat.metap$teachexpect
   schweder(teachexpect)
}
\keyword{ hplot }
