\name{wilkinsonp}
\alias{wilkinsonp}
\alias{maximump}
\alias{minimump}
\alias{Tippett's method}
\alias{print.wilkinsonp}
\alias{print.maximump}
\alias{print.minimump}
\title{Combine p-values using Wilkinson's method}
\description{Combine \eqn{p}-values using Wilkinson's method}
\usage{
wilkinsonp(p, r = 1, alpha = 0.05)
maximump(p, alpha = 0.05)
minimump(p, alpha = 0.05)
\method{print}{wilkinsonp}(x, ...)
\method{print}{maximump}(x, ...)
\method{print}{minimump}(x, ...)
}
\arguments{
  \item{p}{A vector of \eqn{p}-values}
  \item{r}{Use the \eqn{r}th  smallest \eqn{p} value}
  \item{alpha}{The significance level}
  \item{x}{An object of class \sQuote{\code{wilkinsonp}}
    or of class \sQuote{\code{maximump}}
    or of class \sQuote{\code{minimump}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Wilkinson originally proposed his method in the context of
simultaneous statistical inference: the probability
of obtaining \eqn{r} or more significant statistics by
chance in a group of \eqn{k}.
The values are obtained from the Beta distribution, see
\code{\link{pbeta}}.

If \code{alpha} is greater than unity
it is assumed to be a percentage. Either values greater than 0.5 (assumed to
be confidence coefficient) or less than 0.5 are accepted.

The values of \eqn{p} should be such that
\eqn{0\le{}p\le{}1}{0<=p<=1} and a warning is issued if that is not
true.
An error results if possibly as a result of deletions
fewer than two studies remain.

\code{maximump} and
\code{minimump} each provide a wrapper for \code{wilkinsonp}
for the special case when \eqn{r = length(p)}
or \eqn{r=1} respectively and each has its own
print method.
The method of minimum \eqn{p} is also known as Tippett'smethod.

The plot method for class \sQuote{\code{metap}}
calls \code{\link{schweder}} on the valid
\eqn{p}-values.
Inspection of the \eqn{p}-values is recommended as extreme values
in opposite directions do not cancel out.
See last example.
This may not be what you want.
}
\value{An object of class \sQuote{\code{wilkinsonp}}
and \sQuote{\code{metap}} or of class \sQuote{\code{maximump}}
and \sQuote{\code{metap}} or of class \sQuote{\code{minimump}}
and \sQuote{\code{metap}},
a list with entries
  \item{p}{The \eqn{p}-value resulting from the meta--analysis}
  \item{pr}{The \eqn{r}th smallest \eqn{p} value used}
   \item{r}{The value of \eqn{r}}
  \item{critp}{The critical value at which the \eqn{r}th value
      would have been significant for the chosen \code{alpha}}
  \item{validp}{The input vector with illegal values removed}
%% ...
}
\references{
Becker, B J. Combining significance levels. In
Cooper, H and Hedges, L V, editors
\emph{A handbook of research synthesis},
chapter 15, pages 215--230.
Russell Sage,
New York, 1994.

Birnbaum, A.
Combining independent tests of significance.
\emph{Journal of the American Statistical Association},
49:559--574, 1954.

Wilkinson, B.
A statistical consideration in psychological research.
\emph{Psychological Bulletin},
48:156--158, 1951.

}

\author{Michael Dewey}
\seealso{
See also \code{\link{schweder}}
}
\examples{
data(beckerp)
minimump(beckerp) # signif = FALSE, critp = 0.0102, minp = 0.016
data(teachexpect)
minimump(teachexpect) # crit 0.0207, note Becker says minp = 0.0011
wilkinsonp(c(0.223, 0.223), r = 2) # Birnbaum, just signif
data(validity)
minimump(validity) # minp = 0.00001, critp = 1.99 * 10^{-4}
minimump(c(0.0001, 0.0001, 0.9999, 0.9999)) # is significant
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
