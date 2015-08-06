\name{pollutant}
\alias{pollutant}
\docType{data}
\title{Water treatment example from BHH2, Ch 5, Question 19}
\description{
  The data are from the first 8 rows of the pollutant water treatment example
  in the book by Box, Hunter and Hunter, 2nd edition, Chapter 5, Question 19.
  
  The data are in coded units where:
  C: -1 is chemical brand A; +1 is chemical brand B
  T: -1 is 72F for treatment temperature; +1 is 100F for the temperature
  S: -1 is No stirring; +1 is with fast stirring
  y: is the pollutant amount in the discharge [lb/day].
  
  The aim is to find treatment conditions that MINIMIZE the amount of pollutant
  discharged each day, where the limit is 10 lb/day.
}
\usage{data(pollutant)}
\format{A data.frame containing 8 observations of 4 variables (C, S, T and y)}
\source{Box, Hunter and Hunter, 2nd edition, Chapter 5, Question 19, page 232.}
\references{Box, G. E. P. and Hunter, J. S. and Hunter, W. G., 2005, \emph{Statistics for 
        Experimenters}, Second edition. Wiley.}
\keyword{datasets}