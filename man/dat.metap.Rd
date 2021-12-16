\name{dat.metap}
\alias{dat.metap}
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
   \code{rosenthal}, \code{teachexpect}, \code{validity},
   and \code{zhang}.
\loadmathjax}
\usage{data(dat.metap)
}
\format{A list with the following elements:
  \describe{
    \item{\code{beckerp}}{
  A vector of length 5
}
    \item{\code{cholest}}{
  A data frame with 34 observations on the following 5 variables.
  \describe{
    \item{\code{ntreat}}{A numeric vector of number treated}
    \item{\code{ncontrol}}{A numeric vector of number in the control group}
    \item{\code{dtreat}}{A numeric vector of number of deaths in the treated group}
    \item{\code{dcontrol}}{A numeric vector of number of deaths in the control group}
    \item{\code{p}}{a numeric vector of one sided \mjseqn{p} values}
  }
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
    \item{\code{t}}{A numeric vector of values of \mjseqn{t}}
    \item{\code{df}}{a numeric vector of degrees of freedom}
    \item{\code{p}}{a numeric vector of one sided \mjseqn{p} values}
  }
  }
  \item{\code{teachexpect}}{A vector of length 19
   }
   \item{\code{validity}}{
   A data frame with 20 observations on the following 3 variables
  \describe{
    \item{\code{n}}{A numeric vector of sample sizes}
    \item{\code{r}}{a numeric vector of correlation coefficients}
    \item{\code{p}}{a numeric vector of one sided \mjseqn{p} values}
  }
   }
   \item{\code{zhang}}{
   A data frame with 22 observations on the following 11 variables
   \describe{
      \item{\code{study}}{Character, the study names}
      \item{\code{smd}}{numeric, the standardised mean difference}
      \item{\code{lo}}{numeric, the lower confidence limit}
      \item{\code{hi}}{numeric, the upper confidence limit}
      \item{\code{ntreat}}{numeric, the treated sample size}
      \item{\code{ncont}}{numeric, the control sample size}
      \item{\code{n}}{numeric, the total sample size}
      \item{\code{phase}}{factor, what phase the patients were in:
         acute, healing, healed}
      \item{\code{sd}}{numeric, the calculated standard deviation}
      \item{\code{z}}{numeric, the calculated z}
      \item{\code{p}}{numeric, the probability associated with z}
   }
   }
}
}
\details{
\describe{
   \item{\code{beckerp}}{Hypothetical \mjseqn{p} values from \insertCite{becker94;textual}{metap}}
   \item{\code{cholest}}{\if{html}{\eqn{}}\mjseqn{p} values from trials
       of interventions for lowering cholesterol
       lowering from \insertCite{sutton00;textual}{metap}}
   \item{\code{edgington}}{Hypothetical \mjseqn{p} values from \insertCite{edgington72a;textual}{metap}}
   \item{\code{mourning}}{Results from a study of mourning practices of Israeli
       youth following the assassination of Itzakh Rabin
       from \insertCite{benjamini00;textual}{metap}}
   \item{\code{naep}}{Results of mathematical achievment scores
      from the National Assessment
      of Educational Progress from \insertCite{benjamini00;textual}{metap}}
   \item{\code{rosenthal}}{Hypothetical example from \insertCite{rosenthal78;textual}{metap}}
   \item{\code{teachexpect}}{\if{html}{\eqn{}}\mjseqn{p}-values from studies of the effect of
      manipulating teacher expectancy on student IQ
      from \insertCite{becker94;textual}{metap}}
   \item{\code{validity}}{Data from studies of validity of
      student ratings of their instructors
      from \insertCite{becker94;textual}{metap}
      including correlations and sample sizes as well as \mjseqn{p}-values}
   \item{\code{zhang}}{Data from trials of exercise training for
      patients with cardiovascular disease from
      \insertCite{zhang16;textual}{metap}
   }
}
}
\author{Michael Dewey}

\note{The \mjseqn{p}-values in \code{cholest} have been re-calculated
and are of higher accuracy than the ones given in the book.}

\references{
\insertAllCited{}
}
\examples{
data(dat.metap)
}
\keyword{datasets}
