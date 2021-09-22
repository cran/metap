\name{allmetap}
\alias{allmetap}
\alias{print.allmetap}
\title{Carry out all or some of the methods in the package}
\description{Enables all or a selected number of the package
methods to be executed on the same dataset\loadmathjax}
\usage{
allmetap(p, method = NULL)
\method{print}{allmetap}(x, digits = 5, ...)
}
\arguments{
  \item{p}{A vector of \mjseqn{p}-values}
  \item{method}{A vector of character containing names of methods or \sQuote{\code{all}}}
  \item{x}{An object of class \sQuote{\code{allmetap}}}
  \item{digits}{How many digits to print?}
  \item{...}{Other arguments to be passed through}
 
}
\details{The function uses each of the methods specified
   on the data-set in \code{p}. If method contains \sQuote{\code{all}}
   then this over--rides any specific method(s) named.
}
\value{Returns an object of class
\sQuote{\code{allmetap}} and \sQuote{\code{data.frame}} containing columns
  \item{p}{The value of p returned}
  \item{valid}{The number of valid p values used for that method}
  \item{eponym}{The eponym, if any, for the method}
   The row names are the names of the methods used.
   If any of the methods cannot be applied to that data--set NA are
   returned for \code{p} and \code{valid}.
  }
\author{Michael Dewey}
\examples{
data(dat.metap)
beckerp <- dat.metap$beckerp
allmetap(beckerp, method = "all")
allmetap(beckerp, method = c("sumz", "sumlog"))
}
\keyword{htest}

