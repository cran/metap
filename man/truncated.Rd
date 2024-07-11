\name{truncated}
\alias{truncated}
\alias{print.truncated}
\alias{summary.truncated}
\alias{plot.truncated}
\title{Truncated product methods}
\description{Implements two procedures for truncated versions
of Fisher's method\loadmathjax}
\usage{
truncated(p, ptrunc = NULL, rtrunc = NULL, ...)
\method{print}{truncated}(x, ...)
\method{summary}{truncated}(object, ...)
\method{plot}{truncated}(x, pparams = list(pchs = c(16, 1),
   pcols = c("black", "black")), ...)
}
\arguments{
  \item{p}{\sigvec}
  \item{ptrunc}{Value of \mjseqn{p} to truncate at. See Details}
  \item{rtrunc}{Which rank \mjseqn{p} to truncate at. See Details}
  \item{x}{An object of class \sQuote{\code{truncated}}}
  \item{object}{An object of class \sQuote{\code{truncated}}}
  \item{\dots}{Other arguments to be passed through}
  \item{pparams}{A list containing a vector \code{pchs} of plotting
     symbols and a vector \code{pcols} of colours}
}
\details{Uses routines from TFisher to implement
   truncated Fisher \insertCite{zaykin07,zhang18}{metap}
   and from mutoss to implement
   rank-truncated Fisher \insertCite{dudbridge03}{metap}

   The print method prints the statistic and its associated
   \mjseqn{p}-value. The summary method also prints how many
   of the input values were actually used. The plot method
   provides an index plot of the \mjseqn{p}-values.
   By default used \mjseqn{p}-values are plotted
   with a black filled circle and those not used with a black
   unfilled circle. The colours and plotting symbols can be changed
   in \code{pparams}. The first element of each vector is for the
   used \mjseqn{p}-values and the second for the unused ones.
}
\value{An object of class \sQuote{\code{truncated}}
a list with entries
  \item{p}{The resultant overall \mjseqn{p}-value}
  \item{stat}{The test statistic}
  \item{validp}{The input vector with illegal values removed}
  \item{ptrunc}{The \mjseqn{p}-value used for truncation}
  \item{trunc}{The  rank value used for truncation}
  \item{n}{A vector containing \code{incl}, the number
     of valid values included, \code{excl}, the number of valid
     values not used}
  \item{method}{The method used}
}
\note{
   Only one of \code{ptrunc} or \code{rtrunc} is returned
   as appropriate. If the function encounters an error
   it returns NA for the value of \code{p} and gives
   a warning message.
   The effect of specifying both \code{ptrunc}
   and \code{rtrunc} is undefined.
}
\references{
\insertAllCited{}
}
\author{
Michael Dewey
}

\seealso{
\code{\link[mutoss]{ranktruncated}}, \code{\link[TFisher]{stat.tpm}},
and \code{\link[TFisher]{p.tpm}}
}
\examples{
data(dat.metap)
validity <- dat.metap$validity
truncated(validity, ptrunc = 0.5)
truncated(validity, rtrunc = 5)
}
\keyword{htest}
