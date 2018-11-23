% (c) Kevin Dunn, 2014-2015.

\name{boilingpot}
\alias{boilingpot}
\docType{data}
\title{Full factorial experiments for stove-top boiling of water.}
\description{
  The data are from boiling water in a pot under various conditions. The response variable, 
  \verb{y}, is the time taken, in minutes to reach 90 degrees Celsius. Accurately measuring 
  the time to actual boiling is hard, hence the 90 degrees Celsius point is used instead.
  
  Three factors are varied in a full factorial manner (the first 8 observations).
  The data are in standard order, however the actual experiments were run in 
  random order. The last 3 rows are runs close to, or interior to the factorial.
  
  Factors varied were:
  
  \itemize{
  
  \item \verb{A} = Amount of water: low level was 500 mL, and high level was 600 mL
  \item \verb{B} = Lid off (low level) or lid on (high level)
  \item \verb{C} = Size of pot used: low level was 2 L, and high level was 3 L.
  }
  Coded values for \verb{A}, \verb{B}, and \verb{C}, should be used in the linear
  regression model analysis, with \verb{-1} representing the low value and \verb{+1} the
  high value.
}
\usage{data(boilingpot)}
\format{A data.frame containing 11 observations of 4 variables (A, B, C, with y as a response variable.)}
\source{A MOOC on Design of Experiments: ``Experimentation for Improvement'', \url{https://learnche.org}}
\keyword{datasets}