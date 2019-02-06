\name{package-data}
\alias{beckerp}
\alias{cholest}
\alias{edgington}
\alias{naep}
\alias{mourning}
\alias{rosenthal}
\alias{teachexpect}
\alias{validity}
\docType{data}
\title{Example data}
\description{
The package contains the following datasets: \code{beckerp},
   \code{cholest}, \code{edgington}, \code{mourning}, \code{naep},
   \code{rosenthal}, \code{teachexpect}, and \code{validity}.
}
\usage{data(beckerp)
data(cholest)
data(edgington)
data(mourning)
data(naep)
data(rosenthal)
data(teachexpect)
data(validity)
}
\format{
  \describe{
    \item{\code{beckerp}}{
  A vector of length 5
}
    \item{\code{cholest}}{
  A vector of length 34
}
   \item{\code{edgington}}{
   A vector of length 7
}
    \item{\code{naep}}{A data frame with 34 observations on the following 2 variables.
  \describe{
    \item{\code{state}}{a factor with levels \code{AL}, \code{AR}, \code{AZ}, \code{CA}, \code{CO}, \code{CT}, \code{DE}, \code{FL}, \code{GA}, \code{HI}, \code{IA}, \code{ID}, \code{IN}, \code{KY}, \code{LA}, \code{MD}, \code{MI}, \code{MN}, \code{NC}, \code{ND}, \code{NE}, \code{NH}, \code{NJ}, \code{NM}, \code{NY}, \code{OH}, \code{OK}, \code{PA}, \code{RI}, \code{TX}, \code{VA}, \code{WI}, \code{WV}, \code{WY},}
    \item{\code{p}}{a numeric vector}
  }}
  \item{\code{mourning}}{ A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{stance}}{a factor with levels \code{No stand}, \code{Opponent}, \code{Supporter}}
    \item{\code{grade}}{a factor with levels \code{G11-12}, \code{G7-8}, \code{G9-10}}
    \item{\code{p}}{a numeric vector}
  }}
   \item{\code{rosenthal}}{
  A data frame with 5 observations on the following 3 variables.
  \describe{
    \item{\code{t}}{A numeric vector of values of \eqn{t}}
    \item{\code{df}}{a numeric vector of degrees of freedom}
    \item{\code{p}}{a numeric vector of one sided \eqn{p} values}
  }
  }
  \item{\code{teachexpect}}{A vector of length 19
   }
   \item{\code{validity}}{A vector of length 20
   }
}
}
\details{
\describe{
   \item{\code{beckerp}}{Hypothetical \eqn{p} values from \insertCite{becker94;textual}{metap}}
   \item{\code{cholest}}{\eqn{p}-values from studies of cholesterol
       lowering from \insertCite{sutton00;textual}{metap}}
   \item{\code{edgington}}{Hypothetical \eqn{p} values from \insertCite{edgington72a;textual}{metap}}
   \item{\code{mourning}}{Results from a study of mourning practices of Israeli
       youth from \insertCite{benjamini00;textual}{metap}}
   \item{\code{naep}}{Results on the National Assessment
      of Educational Progress from \insertCite{benjamini00;textual}{metap}}
   \item{\code{rosenthal}}{Hypothetical example from \insertCite{rosenthal78;textual}{metap}}
   \item{\code{teachexpect}}{\eqn{p}-values from studies of the effect of
      manipulating teacher expectancy from \insertCite{becker94;textual}{metap}}
   \item{\code{validity}}{\eqn{p}-values from studies of validity of
      student ratings from \insertCite{becker94;textual}{metap}}
   }
}

\references{
\insertAllCited{}
}

\keyword{datasets}
