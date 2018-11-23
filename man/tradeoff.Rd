% http://cran.r-project.org/doc/manuals/r-release/R-exts.html

\name{tradeoff}
\alias{tradeoff}
\alias{tradeOff}
\title{Trade-offs for a specified fractional factorial design}
\description{Fractional factorial designs are a trade-off of information learned \emph{vs.} the amount of work and cost (experiments) invested. This function shows which factors are confounded (confused; aliased) with each other when running a fractional factorial. The output shows the design's resolution and how to generate the fractional factorial.}

\usage{tradeoff(runs=8, factors=7, display=TRUE)}

\arguments{

\item{runs}{
  the number of (unreplicated) experimental runs in the fractional factorial design. Must be a power of 2 that is 8 or higher (e.g. 8, 16, 32, 64, ...)
}
\item{factors}{
  the number of factors being investigated in the fractional factorial design.
}
\item{display}{
  will by default print the results to the screen and also returns a list containing the same information; setting this to \code{FALSE} will suppress the screen output.
}
}


\details{Fractional factorial designs require a sacrifice in the clarity of the information learned, at the benefit of performing fewer
experimental runs; thereby decreasing cost and time to run the full set of factorial experiments. See \code{\link{tradeOffTable}} for a
visual display of this concept.

For example, when running 8 experiments with 7 factors, the design resolution (clarity of estimated effects) is going to be low. This function's
output indicates how the factors are aliased (confounded) with each other, so you can evaluate the use of the design before actually performing it.

The function provides 3 main sources of information: the resolution, the generators and the aliasing structure. Each one of these is described in
the reference.

Aliasing is only reported to the level of the main effects and two-factor interaction (2fi, \code{fi2}). Higher level interactions are of-course
present in many fractional factorial designs, and may be calculated from the defining relationship. Future versions of this function will
return the defining relationship to assist with this.
}

% Description of the function's return value.
\value{The function currently returns a list with 3 entries: the resolution, the generator(s) and the aliasing structure.
}

% A section with references to the literature. Use \url{} or \href{}{} for web pointers.
\references{Please see Chapter 5 of the following book: \cr
Kevin Dunn, 2010 to 2019, \emph{Process Improvement using Data}, \url{https://learnche.org/pid}

Please see this paper to gain an understanding of how these trade-off tables are constructed:\cr
Arthur Fries and William G. Hunter, (1980) Minimum Aberration \eqn{2^{k-p}} Designs, \emph{Technometrics}, \bold{22}(4), pp. 601-608, \url{https://www.jstor.org/stable/1268198}
}

% Use this for a special note you want to have pointed out. Multiple \note sections are allowed, but might be confusing to the end users.
%\note{}

\author{Kevin Dunn, <kgdunn@gmail.com>}

\seealso{\code{\link{tradeOffTable}} for a visual representation of this information.}
\examples{

# Running 8 experiments? What are the trade-offs with 4, 5, 6, or 7 factors?
\dontrun{tradeoff(runs=8, factors=4)}
\dontrun{tradeoff(runs=8, factors=5)}
\dontrun{tradeoff(runs=8, factors=6)}
\dontrun{tradeoff(runs=8, factors=7)}

# Running 16 experiments? What are the trade-offs ?
\dontrun{tradeoff(runs=16, factors=5)}
\dontrun{tradeoff(runs=16, factors=6)}
\dontrun{tradeoff(runs=16, factors=7)}
\dontrun{tradeoff(runs=16, factors=8)}
\dontrun{tradeoff(runs=16, factors=9)}
}

%Examples are not only useful for documentation purposes, but also provide test code used for diagnostic checking of R code. By default, text inside \examples{} will be displayed in the output of the help page and run by example() and by R CMD check. You can use \dontrun{} for text that should only be shown, but not run, and \dontshow{} for extra commands for testing that should not be shown to users, but will be run by example(). (Previously this was called \testonly, and that is still accepted.)

% Text inside \dontrun{} is 'verbatim', but the other parts of the \examples section are R-like text.

%For example,

%x <- runif(10)       # Shown and run.
%\dontrun{plot(x)}    # Only shown.
%\dontshow{log(x)}    # Only run.
%Thus, example code not included in \dontrun must be executable! In addition, it should not use any system-specific features or require special facilities (such as Internet access or write permission to specific directories). Text included in \dontrun is indicated by comments in the processed help files: it need not be valid R code but the escapes must still be used for %, \ and unpaired braces as in other verbatim text.

\concept{ design of experiments }
