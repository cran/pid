\name{pid-package}
\alias{pid-package}
\docType{package}
\title{
Process Improvement using Data
}
\description{
This package contains functions and datasets that complement the book
Process Improvement using Data, \url{https://learnche.org/pid}.

The functions and datasets are also used in the massive open online course (MOOC) called
"Experimentation for Improvement", hosted on Coursera, \url{https://yint.org/experiments}.

The functions are most useful for design of experiments (DOE), and additional functions
and datasets will be added as the book and the MOOC evolve over time.
}
\details{
The package is still subject to development, in terms of datasets and functionality,
at least until version numbers exceed 1.0. Version numbers below 1.0 indicate that
functionality may be added, removed or changed over time.

Please contact me, if you have suggestions.

Other packages that you can use immediately for experimental design are: \pkg{\link[FrF2:FrF2-package]{FrF2}} for fractional factorial experiments with 2-level factors and the base package for Designed Experiments, called \pkg{\link[DoE.base:DoE.base-package]{DoE.base}}.
}
\author{
Kevin Dunn

Maintainer: Kevin Dunn <kgdunn@gmail.com>
}
\references{
    Box G. E. P, Hunter, W. C. and Hunter, J. S. (2005)
    \emph{Statistics for Experimenters, 2nd edition}.
    New York: Wiley.
}
\keyword{array}
\keyword{design}
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
