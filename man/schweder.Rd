\name{schweder}
\alias{schweder}
\title{Schweder and Spjotvoll plot}
\description{Produces the plot suggested by Schweder and Spjotvol to display
a collection of \eqn{p}-values and also optionally draws the
lowest slope line suggested by Benjamini and Hochberg
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
  \item{p}{A vector of \eqn{p}-values}
  \item{xlab}{Label for plot \eqn{x}-axis}
  \item{ylab}{Label for plot \eqn{y}-axis}
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
\details{After removing invalid \eqn{p}-values plots them.
Any graphics parameters passed in \code{...} affect this part
of the plot only.
Note that the axes are chosen according to the scheme of Banjamini and
Hochberg, Schweder and Spjotvol used a different system.

By setting \code{drawline} appropriately up to three
lines may be drawn.

If \code{drawline} includes the string \code{"bh"}
the lowest slope line of Benjaimin and Hochberg is drawn.
No further parameters are needed here but the characteristics of the
line may be set: width, line type, and colour.

If \code{drawline} includes the string \code{"ls"}
a least squares line is drawn passing through the point
\eqn{k+1, 1}.
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
  \item{p}{The values of \eqn{p} plotted}
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
Benjamini, Y and Hochberg, Y.
On the adaptive control of the false disovery rate in
multiple testing with independent statistics.
\emph{Journal of Educational and Behavioral Statistics},
25: 60--83, 2000.

   Schweder, T and Spjotvoll, E.
   Plots of \eqn{P}--values to evaluate many tests simultaneously.
  \emph{Biometrika}, 69:493--502, 1982.
}
\author{Michael Dewey}

%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
   data(teachexpect)
   schweder(teachexpect)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
