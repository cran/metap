\name{sumlog}
\alias{sumlog}
\alias{Fisher's method}
\alias{chi-square (2) method}
\alias{print.sumlog}
\title{Combine p-values by the sum of logs (Fisher's) method
}
\description{Combine \mjseqn{p}-values by the sum of logs method,
also known as Fisher's method, and sometimes as the
chi-square (2) method\loadmathjax}
\usage{
sumlog(p, log.p = FALSE)
\method{print}{sumlog}(x, ...)
}
\arguments{
  \item{p}{\sigvec}
  \item{log.p}{\logp}
  \item{x}{An object of class \sQuote{\code{sumlog}}}
  \item{...}{Other arguments to be passed through}
}
\details{
The method relies on the fact that
\mjdeqn{\sum_{i=1}^k - 2 \log p_i}{sum (-2 log p)}
is a chi-squared with \mjeqn{2 k}{2 * k} df where \mjseqn{k} is the number
of studies \insertCite{fisher25}{metap}.
\insertNoCite{becker94}{metap}
\insertNoCite{rosenthal78}{metap}
\insertNoCite{sutton00}{metap}

\ltle{two}

\plotmethod
\nocancel
}
\value{
An object of class \sQuote{\code{sumlog}} and \sQuote{\code{metap}},
a list with entries
  \item{chisq}{Value of chi-squared statistic}
  \item{df}{Associated degrees of freedom}
  \item{p}{Associated \mjseqn{p}-value}
  \item{validp}{The input vector with the illegal values removed}
%% ...
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
teachexpect <- dat.metap$teachexpect
sumlog(teachexpect) # chisq = 69.473, df = 38, p = 0.0014, from Becker
beckerp <- dat.metap$beckerp
sumlog(beckerp) # chisq = 18.533, df = 10, sig
rosenthal <- dat.metap$rosenthal
sumlog(rosenthal$p) # chisq = 22.97, df = 10, p = 0.006 one sided
cholest <- dat.metap$cholest
sumlog(cholest) # chisq = 58.62, df = 68, p = 0.78
validity <- dat.metap$validity
sumlog(validity) # chisq = 159.82, df = 40, p = 2.91 * 10^{-16}
sumlog(c(0.0001, 0.0001, 0.9999, 0.9999)) # is significant
all.equal(exp(sumlog(validity, log.p = TRUE)$p), sumlog(validity)$p)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
