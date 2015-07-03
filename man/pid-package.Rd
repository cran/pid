\name{pid-package}
\alias{pid-package}
\docType{package}
\title{
Process Improvement using Data
}
\description{
This package contains functions and datasets that complement the book
Process Improvement using Data.

The functions and datasets are also used in the massive open online course (MOOC) called
Experimentation for Improvement, hosted on Coursera.

The functions are most useful for design of experiments (DOE), and more functions will be added
in later versions.
}
\details{
The package is still subject to extensive development in terms of extra datasets and functionality
that is planned for addition.

Please contact me, if you have suggestions.

Other packages that you can use immediately for experimental design are: \pkg{\link[FrF2:FrF2-package]{FrF2}} for fractional factorial experiments with 2-level factors and the base package for Designed Experiments, called \pkg{\link[DoE.base:DoE.base-package]{DoE.base}}.
}
\author{
Kevin Dunn

Maintainer: Kevin Dunn <kevin.dunn@mcmaster.ca>
}
\references{
    Box G. E. P, Hunter, W. C. and Hunter, J. S. (2005) 
    \emph{Statistics for Experimenters, 2nd edition}.
    New York: Wiley.
    
    Daniel, C. (1959) Use of Half Normal Plots in Interpreting Two Level Experiments.
    \emph{Technometrics}, \bold{1}, 311-340.
    
    Lenth, R.V. (1989) Quick and easy analysis of unreplicated factorials.
    \emph{Technometrics}, \bold{31}, 469-473.
  
}
\keyword{ array }
\keyword{ design }
\seealso{
Related packages: 
  \pkg{\link[DoE.base:DoE.base-package]{DoE.base}}, \pkg{\link[BsMD:BsMD-package]{BsMD}}, \pkg{\link[FrF2:FrF2-package]{FrF2}}
}
\examples{
	# 2-factor example
	T <- c(-1, +1, -1, +1)  # centered and scaled temperature
	S <- c(-1, -1, +1, +1)  # centered and scaled speed variable
	y <- c(69, 60, 64, 53)  # conversion, is our response variable, y
	doe.model <- lm(y ~ T + S + T * S)  # create a model with main effects, and interaction
	paretoPlot(doe.model)  

	# 3-factor example
	data(pollutant)
	mod.full <- lm(y ~ C*T*S, data=pollutant) 
	paretoPlot(mod.full)       
}
