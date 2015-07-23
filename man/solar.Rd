\name{solar}
\alias{solar}
\docType{data}
\title{Solar panel example from BHH2, Chapter 5, page 230}
\description{
  The data are from a solar panel simulation case study.
  
  The original source that Box, Hunter and Hunter used is 
  \url{http://www.sciencedirect.com/science/article/pii/0038092X67900515}
  
  A theoretical model for a commercial system was made. A 2^4 factorial 
  design was used (center point is not included in this dataset).
  
  The factors are dimensionless groups (\url{https://en.wikipedia.org/wiki/Dimensionless_quantity})
  related to 
  \verb{A}: total daily insolation, 
  \verb{B}: the tank capacity, 
  \verb{C}: the water flow through the absorber, 
  \verb{D}: solar intermittency coming in.
  
  The responses variables are 
  \verb{y1}: collection efficiency, and 
  \verb{y2}: the energy delivery efficiency.
  
  The coded values for \verb{A}, \verb{B}, \verb{C}, and \verb{D} are given, with -1 representing
  the low value and +1 the high value.
}
\usage{data(solar)}
\format{A data.frame containing 16 observations of 6 variables (A, B, C, D, with y1 and y2 as responses.)}
\source{Box, Hunter and Hunter, 2nd edition, Chapter 5, page 230.}
\references{Box, G. E. P. and Hunter, J. S. and Hunter, W. G., 2005, \emph{Statistics for 
        Experimenters}, Second edition. Wiley."}
\keyword{datasets}