% http://cran.r-project.org/doc/manuals/r-release/R-exts.html

\name{paretoPlot}
%name typically is the basename of the Rd file containing the documentation. It is the "name" of the Rd object represented by the file and has to be unique in a package. To avoid problems with indexing the package manual, it may not contain `, | nor @ , and to avoid possible problems with the HTML help system it should not contain '/' nor a space. (LaTeX special characters are allowed, but may not be collated correctly in the index.) There can only be one \name entry in a file, and it must not contain any markup. Entries in the package manual will be in alphabetic order of the \name entries.

\alias{paretoPlot}
%The \alias sections specify all "topics" the file documents. This information is collected into index data bases for lookup by the on-line (plain text and HTML) help systems. The topic can contain spaces, but (for historical reasons) leading and trailing spaces will be stripped. Percent and left brace need to be escaped by a backslash.

\alias{pareto.plot}

\title{Pareto plot (coefficient plot) for a factorial design model}
%Title information for the Rd file. This should be capitalized and not end in a period; try to limit its length to at most 65 characters for widest compatibility.
% There must be one (and only one) \title section in a help file.

\description{Creates a plot that shows the model coefficients from the least squares model as bars. 
The absolute value of the coefficients are taken. 
The coefficient's sign is represented by the bar colour: grey for negative and black for positive coefficients.}
%A short description of what the function(s) do(es) (one paragraph, a few lines only). (If a description is too long and cannot easily be shortened, the file probably tries to document too much at once.) This is mandatory except for package-overview files.

\usage{paretoPlot(lsmodel, xlab="Effect name", ylab="Magnitude of effect",
                       main="Pareto plot", legendtitle="Sign of coefficient",
                       negative=c("Negative", "grey"),
                       positive=c("Positive", "black"))} 
%One or more lines showing the synopsis of the function(s) and variables documented in the file. These are set in typewriter font. This is an R-like command.
% The usage information specified should match the function definition exactly (such that automatic checking for consistency between code and documentation is possible).

% A key:value list of input arguments.
\arguments{

\item{lsmodel}{
  a linear model object (least squares model) created by the \code{lm(\ldots)} function.
}
\item{xlab}{
  label for horizontal axis in the bar plot.
}
\item{ylab}{
  label for vertical axis in the bar plot.
}
\item{main}{
  label for the plot.
}
\item{legendtitle}{
  an alternative to the default legend title.
}
\item{negative}{
  a two-element vector, both entries must be strings, the first of which 
  provides the name for negative bars in the legend, and the second tells which
  colour to plot negative bars with. The default is: c("Negative", "grey").
}
\item{positive}{
  a two-element vector, both entries must be strings, the first of which 
  provides the name for positive bars in the legend, and the second tells which
  colour to plot those bars with.
}
}

% A detailed if possible precise description of the functionality provided, extending the basic information in the \description slot.
\details{Typical usage is to create a generic linear model with the \code{lm(\dots)} command, and supply that as the input to this function.

For example, a general design of experiments with 4 factors: A, B, C, and D can be built using \code{lsmodel <- lm(y ~ A*B*C*D)}, and then the 2^4=16 coefficients visualized with this function using \code{paretoPlot(lsmodel)}. Since the largest magnitude coefficients are mostly of interest, the coefficient bars are shown sorted from largest to smallest absolute magnitude. The sign information is retained though with the bar's colour: grey for negative and black for positive coefficients.

The coefficients are the exact coefficients from the linear model. When the linear model is in coded units (i.e. -1 for the low level, and +1 for the high level), then the coefficients represent the change in the 
experiment's response variable (y), for a 2-unit (not 1-unit) change in the input variable. This interpretation of course assumes the factors are independent, which is the case in a full factorial. 

Please see the reference for more details. 
}

% Description of the function's return value.
\value{Returns a \code{ggplot2} object, which, by default, is shown before being returned by this function. The \code{ggplot2} object may be further manipulated, if desired.}

% A section with references to the literature. Use \url{} or \href{}{} for web pointers.
\references{A Pareto plot is similar in spirit to the Lenth plot, other than for the fact that absolute coefficients are shown. Please see Chapter 5 of the following book:
Kevin Dunn, 2010 to 2015, \emph{Process Improvement using Data}, \url{http://learnche.org/pid}
}

% Use this for a special note you want to have pointed out. Multiple \note sections are allowed, but might be confusing to the end users.
%\note{}

\author{Kevin Dunn, <kevin.dunn@mcmaster.ca>}
%Information about the author(s) of the Rd file. Use \email{} without extra delimiters to specify email addresses, or \url{} or \href{}{} for web pointers.

%\seealso{}
%Pointers to related R objects, using \code{\link{...}} to refer to them (\code is the correct markup for R object names, and \link produces hyperlinks in output formats which support this. See Marking text, and Cross-references).
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

%Examples are not only useful for documentation purposes, but also provide test code used for diagnostic checking of R code. By default, text inside \examples{} will be displayed in the output of the help page and run by example() and by R CMD check. You can use \dontrun{} for text that should only be shown, but not run, and \dontshow{} for extra commands for testing that should not be shown to users, but will be run by example(). (Previously this was called \testonly, and that is still accepted.)

% Text inside \dontrun{} is 'verbatim', but the other parts of the \examples section are R-like text.

%For example,

%x <- runif(10)       # Shown and run.
%\dontrun{plot(x)}    # Only shown.
%\dontshow{log(x)}    # Only run.
%Thus, example code not included in \dontrun must be executable! In addition, it should not use any system-specific features or require special facilities (such as Internet access or write permission to specific directories). Text included in \dontrun is indicated by comments in the processed help files: it need not be valid R code but the escapes must still be used for %, \ and unpaired braces as in other verbatim text.

\concept{ design of experiments }
