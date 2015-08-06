% http://cran.r-project.org/doc/manuals/r-release/R-exts.html

\name{contourPlot}
%name typically is the basename of the Rd file containing the documentation. It is the "name" of the Rd object represented by the file and has to be unique in a package. To avoid problems with indexing the package manual, it may not contain `, | nor @ , and to avoid possible problems with the HTML help system it should not contain '/' nor a space. (LaTeX special characters are allowed, but may not be collated correctly in the index.) There can only be one \name entry in a file, and it must not contain any markup. Entries in the package manual will be in alphabetic order of the \name entries.

\alias{contourPlot}
%The \alias sections specify all "topics" the file documents. This information is collected into index data bases for lookup by the on-line (plain text and HTML) help systems. The topic can contain spaces, but (for historical reasons) leading and trailing spaces will be stripped. Percent and left brace need to be escaped by a backslash.


\title{2-dimensional contour plot of a factorial design model}
%Title information for the Rd file. This should be capitalized and not end in a period; try to limit its length to at most 65 characters for widest compatibility.
% There must be one (and only one) \title section in a help file.

\description{Creates a two-dimensional contour plot of a factorial design model (linear least squares model). The two specified factors are
plotted on the horizontal and vertical axes. If the model has more than two factors, then the remaining factors are set at their zero level.
It is assumed the model is in coded units.}
%A short description of what the function(s) do(es) (one paragraph, a few lines only). (If a description is too long and cannot easily be shortened, the file probably tries to document too much at once.) This is mandatory except for package-overview files.

\usage{contourPlot(lsmodel, 
            xlab=attr(lsmodel$terms,'term.labels')[1], 
            ylab=attr(lsmodel$terms,'term.labels')[2], 
            main="Contour plot", 
            N=25, 
            xlim=c(-3.2, 3.2), 
            ylim=c(-3.2, 3.2),
            colour.function=terrain.colors)} 
%One or more lines showing the synopsis of the function(s) and variables documented in the file. These are set in typewriter font. This is an R-like command.
% The usage information specified should match the function definition exactly (such that automatic checking for consistency between code and documentation is possible).

% A key:value list of input arguments.
\arguments{

\item{lsmodel}{
  a linear model object (least squares model) created by the \code{lm(\ldots)} function.
}
\item{xlab}{
  the label and variable used for horizontal axis in the bar plot, and \bold{must} also match a variable in the \code{lsmodel}; the default will use the first available variable.
}
\item{ylab}{
  the label and variable used for vertical axis in the bar plot, and \bold{must} also match a variable in the \code{lsmodel}; the default will use the second available variable.
}
\item{main}{
  label for the plot.
}
\item{N}{
  the resolution of the plot; higher values have greater resolution.
}
\item{xlim}{
  the limits of the horizontal axis (in coded units); the defaults indicate the model's effect outside the standard factorial range.
}
\item{ylim}{
  the limits of the vertical axis (in coded units); the defaults indicate the model's effect outside the standard factorial range.
}
\item{colour.function}{
  the function used to colour-code the contour plot; see \code{\link{rainbow}} for alternative colour schemes.
}
}

% A detailed if possible precise description of the functionality provided, extending the basic information in the \description slot.
\details{Typical usage is to create a generic linear model with the \code{lm(\dots)} command, and supply that as the input to this function.

For example, a general design of experiments with 4 factors: A, B, C, and D can be built using\cr
\code{lsmodel <- lm(y ~ A*B*C*D)}, and then various 2-D contour plots visualized with\cr
\code{contourPlot(lsmodel, "A", "B")}, \code{contourPlot(lsmodel, "B", "C")}, or whichever combination of factors
are desired. The variables \emph{not} being plotted are set at their zero (0) value. 

Future versions of this function will allow specifying the inactive variable levels. In the interim, please see the \code{rsm} 
package (the \code{rsm::contour(...)} function) if this functionality is required.
}

% Description of the function's return value.
\value{Returns a \code{ggplot2} object, which, by default, is shown before being returned by this function. 
The \code{ggplot2} object may be further manipulated, if desired.}

% A section with references to the literature. Use \url{} or \href{}{} for web pointers.
\references{Please see Chapter 5 of the following book:
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
# ----------------
T <- c(-1, +1, -1, +1)  # centered and scaled temperature
S <- c(-1, -1, +1, +1)  # centered and scaled speed variable
y <- c(69, 60, 64, 53)  # conversion, is our response variable, y
doe.model <- lm(y ~ T + S + T * S)  # model with main effects, and interaction
contourPlot(doe.model)  

# 3-factor example
# ----------------
data(pollutant)
mod.full <- lm(y ~ C*T*S, data=pollutant) 
contourPlot(mod.full, N=50)               # high resolution plot
contourPlot(mod.full, xlab='C', ylab='S',
            main="Strong C:S interaction",
			colour.function=rainbow)
			
# Central composite design
P <- c(-1,   +1,  -1,  +1,     0, -1.41,     0, +1.41)
T <- c(-1,   -1,  +1,  +1, +1.41,     0, -1.41,     0)
y <- c(715, 713, 733, 725,   738,   717,   721,   710)
mod.CCD <- lm(y ~ P*T + I(P^2) + I(T^2))  
contourPlot(mod.CCD, 'P', 'T', xlim=c(-2.2, 2.2), ylim=c(-3,2))
}

%Examples are not only useful for documentation purposes, but also provide test code used for diagnostic checking of R code. By default, text inside \examples{} will be displayed in the output of the help page and run by example() and by R CMD check. You can use \dontrun{} for text that should only be shown, but not run, and \dontshow{} for extra commands for testing that should not be shown to users, but will be run by example(). (Previously this was called \testonly, and that is still accepted.)

% Text inside \dontrun{} is 'verbatim', but the other parts of the \examples section are R-like text.

%For example,

%x <- runif(10)       # Shown and run.
%\dontrun{plot(x)}    # Only shown.
%\dontshow{log(x)}    # Only run.
%Thus, example code not included in \dontrun must be executable! In addition, it should not use any system-specific features or require special facilities (such as Internet access or write permission to specific directories). Text included in \dontrun is indicated by comments in the processed help files: it need not be valid R code but the escapes must still be used for %, \ and unpaired braces as in other verbatim text.

\concept{ design of experiments }
