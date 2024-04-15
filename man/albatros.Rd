\name{albatros}
\alias{albatros}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Albatros plot}
\description{Produces the albatros plot of Harrison et al\loadmathjax}
\usage{
albatros(p, n, axes = list(xlimit = NULL, ylimit = NULL,
      lefttext = NULL, righttext = NULL),
   contours = list(type = NULL, contvals = NULL, ltys = NULL, contlabs = NULL),
   plotpars = list(yscale = NULL, pchs = NULL, cols = NULL), ...)
}
\arguments{
  \item{p}{A vector of \mjseqn{p} values}
  \item{n}{A vector of sample sizes}
  \item{axes}{A list containing
     \itemize{
     \item \code{xlimit} a single value, the limit
     for the x-axis which is used symmetrically
     \item \code{ylimit} a vector of length 2, limits for the y-axis
     \item \code{lefttext} character, to label below plot
     \item \code{righttext} character, to label below plot
     }
  }
  \item{contours}{A list containing
     \itemize{
     \item \code{type} what effect size to use, a character one of "corr",
        "or", "smd"
     \item \code{contvals} a vector, what values to draw the contours at
     \item \code{ltys} a vector, what line types to use
     \item \code{contlabs} logical, draw the labels of the countours?
     }
  }
  \item{plotpars}{A list containing
      \itemize{
      \item \code{yscale} Scaling for y-axis, See details
      \item \code{pchs} Symbols to be used
      \item \code{cols} Colours for points
      }
  }
  \item{\dots}{Arguments to be passed through to \code{plot}}
}
\details{Plots \mjseqn{n} against \mjseqn{p} and draws contours of
constant effect size.
\insertNoCite{harrison17}{metap}

The \mjseqn{p} value scale on the x-axis is a logarithmic one
but folded at the null value so that the extreme values represent
small probabilities in each direction.
The scale for the y-axis is user selectable.
The original scale in the Stata version is
\mjeqn{(\log_{10}n)^2}{log(n, base = 10) ^ 2} which is obtained by
setting \code{yscale} to "classic"
but it is
also possible to use \mjeqn{\sqrt{n}}{sqrt(n)}
which is the default or \mjseqn{n} by setting
\code{yscale} to "n".
If the dataset contains extreme values the
plot may be uninformative and the \code{xlimit}
and \code{ylimit} parameters my be helpful to clip
the plot.
If they are not specified the function tries
to choose suitable values.

For the contours if the \code{type} parameter in \code{contours}
is NULL (the default) no contours will be plotted.
The options available are
\dQuote{smd} (Standardised mean difference)
\dQuote{corr} (Correlation)
\dQuote{or} (Odds ratio).

The contours are labelled unless contlabs is set to \code{FALSE}.
The function tries to position these so they are not obscured
by the contour lines but if that is unsatisfactory then
it is best to suppress them and position them manually
or use a legend to identify line types.

The \code{pch} parameter may either be a single value
or a vector of the same length as \code{p}.
It may contain anything which is legal as input to
the parameter of the same name in the graphics
library and will be used for the plotted points.
The \code{cols} parameter works similarly.

The values of \code{lefttext} and \code{righttext} are
used to produce labels under the \mjseqn{x}-axis and
are placed level with the label.
By default they are not produced.

The return values from the function may be useful if further
annotations are required as they give the axis limits in
plotting units.
}
\value{Returns
a list containing
  \item{xlims}{The limits for the x-axis in plotting units}
  \item{ylims}{The limits for the y-axis in plotting units}
}
\references{
\insertAllCited{}
}
\author{Michael Dewey}
\note{The docontours function is for internal use only}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{For plotting parameters
see \code{\link{par}}
}
\examples{
data(dat.metap)
validity <- dat.metap$validity
fit.v <- albatros(validity$p, validity$n,
   contours = list(type = "corr", contvals = c(0.25, 0.5, 0.8), ltys = 1:3),
      axes = list(ylimit = c(1,200),  lefttext = "Negative correlation",
         righttext = "Positive correlation"),
   main = "Validity")
#
fit.z <- albatros(dat.metap$zhang$p, dat.metap$zhang$n,
   contours = list(type = "smd", contvals = c(0.25, 0.5, 1), ltys = 1:3),
   plotpars = list(pchs = letters[unclass(dat.metap$zhang$phase)]),
   axes = list(lefttext = "Favours control", righttext = "Favours exercise"),
   main = "Zhang"
   )
}
\keyword{hplot}
