\name{golf}
\alias{golf}
\docType{data}
\title{Full factorial experiments to maximize a golfer's driving distance.}
\description{
  
  A full factorial experiment with four factors run by a golf enthusiast. The 
  objective of the experiments was for the golfer to maximize her driving distance 
  at a specific tee off location on her local golf course. The golfer considered 
  the following factors:

  \itemize{
  \item \verb{T} = Tee height (cm)
  \item \verb{H} = Holes the golf ball had been played for prior to the experimental tee shot
  \item \verb{C} = Club type
  \item \verb{D} = Time of day (on the 24 hour clock)
  }
  
  The data are in standard order, however the actual experiments were run in 
  random order. 
  
  Coded values for \verb{A}, \verb{B}, \verb{C}, and \verb{D} should be used in the linear
  regression model analysis, with \verb{-1} representing the low value and \verb{+1} the
  high value.
}
\usage{data(golf)}
\format{A data.frame containing 16 observations of 4 variables (A, B, C, D, with y as a response variable.)}
\source{A MOOC on Design of Experiments: ``Experimentation for Improvement'', \url{http://learnche.org}}
\keyword{datasets}