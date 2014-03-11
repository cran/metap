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
    \item{\code{state}}{a factor with levels \code{AL} \code{AR} \code{AZ} \code{CA} \code{CO} \code{CT} \code{DE} \code{FL} \code{GA} \code{HI} \code{IA} \code{ID} \code{IN} \code{KY} \code{LA} \code{MD} \code{MI} \code{MN} \code{NC} \code{ND} \code{NE} \code{NH} \code{NJ} \code{NM} \code{NY} \code{OH} \code{OK} \code{PA} \code{RI} \code{TX} \code{VA} \code{WI} \code{WV} \code{WY}}
    \item{\code{p}}{a numeric vector}
  }}
  \item{\code{mourning}}{ A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{stance}}{a factor with levels \code{No stand} \code{Opponent} \code{Supporter}}
    \item{\code{grade}}{a factor with levels \code{G11-12} \code{G7-8} \code{G9-10}}
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
   \item{\code{beckerp}}{Hypothetical \eqn{p} values from Becker}
   \item{\code{cholest}}{\eqn{p}-values from studies of cholesterol
       lowering from Sutton}
   \item{\code{edgington}}{Hypothetical \eqn{p} values from Edgington}
   \item{\code{mourning}}{Results from a study of mourning practices of Israeli
       youth from Benjamini}
   \item{\code{naep}}{Results on the National Assessment
      of Educational Progress from Benjamini}
   \item{\code{rosenthal}}{Hypothetical example from Rosenthal}
   \item{\code{teachexpect}}{\eqn{p}-values from studies of the effect of
      manipulating teacher expectancy from Becker}
   \item{\code{validity}}{\eqn{p}-values from studies of validity of
      student ratings from Becker}
   }
}
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

Edgington, E S.
An additive method for combining probability values from
independent experiments.
\emph{Journal of Psychology},
80:351-363, 1972.

Rosenthal, R. Combining the results of independent studies.
\emph{Psychological Bulletin},
85:185--193, 1978.

Sutton A J, Abrams, K R, Jones D R, Sheldon T A and Song, F.
\emph{Methods for meta-analysis in medical research}.
Wiley, Chichester, 2000.


}
\keyword{datasets}
