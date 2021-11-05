\name{wilkinsonp}
\alias{wilkinsonp}
\alias{maximump}
\alias{minimump}
\alias{Tippett's method}
\alias{print.wilkinsonp}
\alias{print.maximump}
\alias{print.minimump}
\title{Combine p-values using Wilkinson's method}
\description{Combine \mjseqn{p}-values using Wilkinson's method\loadmathjax}
\usage{
wilkinsonp(p, r = 1, alpha = 0.05, log.p = FALSE)
maximump(p, alpha = 0.05, log.p = FALSE)
minimump(p, alpha = 0.05, log.p = FALSE)
\method{print}{wilkinsonp}(x, ...)
\method{print}{maximump}(x, ...)
\method{print}{minimump}(x, ...)
}
\arguments{
  \item{p}{\sigvec}
  \item{r}{Use the \mjseqn{r}th  smallest \mjseqn{p} value}
  \item{alpha}{The significance level}
  \item{log.p}{\logp}
  \item{x}{An object of class \sQuote{\code{wilkinsonp}}
    or of class \sQuote{\code{maximump}}
    or of class \sQuote{\code{minimump}}}
  \item{...}{Other arguments to be passed through}
}
\details{
Wilkinson \insertCite{wilkinson51}{metap}
originally proposed his method in the context of
simultaneous statistical inference: the probability
of obtaining \mjseqn{r} or more significant statistics by
chance in a group of \mjseqn{k}.
The values are obtained from the Beta distribution, see
\code{\link{pbeta}}.

If \code{alpha} is greater than unity
it is assumed to be a percentage. Either values greater than 0.5 (assumed to
be confidence coefficient) or less than 0.5 are accepted.

\lele{two}

\code{maximump} and
\code{minimump} each provide a wrapper for \code{wilkinsonp}
for the special case when \mjeqn{r = \mathrm{length}(p)}{r = length(p)}
or \mjseqn{r=1} respectively and each has its own
print method.
The method of minimum \mjseqn{p} is also known as Tippett's method
\insertCite{tippett31}{metap}.
\insertNoCite{becker94}{metap}\insertNoCite{birnbaum54}{metap}

\plotmethod
\nocancel
}
\value{An object of class \sQuote{\code{wilkinsonp}}
and \sQuote{\code{metap}} or of class \sQuote{\code{maximump}}
and \sQuote{\code{metap}} or of class \sQuote{\code{minimump}}
and \sQuote{\code{metap}},
a list with entries
  \item{p}{The \mjseqn{p}-value resulting from the meta--analysis}
  \item{pr}{The \mjseqn{r}th smallest \mjseqn{p} value used}
   \item{r}{The value of \mjseqn{r}}
  \item{critp}{The critical value at which the \mjseqn{r}th value
      would have been significant for the chosen \code{alpha}}
  \item{validp}{The input vector with illegal values removed}
%% ...
}
\references{
\insertAllCited{}
}

\note{The value of \code{critp} is always on the raw scale even
if \code{log.p} has been set to TRUE}

\author{Michael Dewey}
\seealso{
See also \code{\link{plotp}}
}
\examples{
data(dat.metap)
beckerp <- dat.metap$beckerp
minimump(beckerp) # signif = FALSE, critp = 0.0102, minp = 0.016
teachexpect <- dat.metap$teachexpect
minimump(teachexpect) # crit 0.0207, note Becker says minp = 0.0011
wilkinsonp(c(0.223, 0.223), r = 2) # Birnbaum, just signif
validity <- dat.metap$validity$p
minimump(validity) # minp = 0.00001, critp = 1.99 * 10^{-4}
minimump(c(0.0001, 0.0001, 0.9999, 0.9999)) # is significant
all.equal(exp(minimump(validity, log.p = TRUE)$p), minimump(validity)$p)
all.equal(exp(maximump(validity, log.p = TRUE)$p), maximump(validity)$p)
}
\keyword{ htest }
