\name{popcorn}
%name typically is the basename of the Rd file containing the documentation. It is the "name" of the Rd object represented by the file and has to be unique in a package. To avoid problems with indexing the package manual, it may not contain `, | nor @ , and to avoid possible problems with the HTML help system it should not contain '/' nor a space. (LaTeX special characters are allowed, but may not be collated correctly in the index.) There can only be one \name entry in a file, and it must not contain any markup. Entries in the package manual will be in alphabetic order of the \name entries.

\alias{popcorn}
%The \alias sections specify all "topics" the file documents. This information is collected into index data bases for lookup by the on-line (plain text and HTML) help systems. The topic can contain spaces, but (for historical reasons) leading and trailing spaces will be stripped. Percent and left brace need to be escaped by a backslash.

\title{Simulation of stovetop popcorn cooking}
%Title information for the Rd file. This should be capitalized and not end in a period; try to limit its length to at most 65 characters for widest compatibility. There must be one (and only one) \title section in a help file.

\description{A fixed number of popcorn kernels are cooked are heated at the same temperature. This simulation returns the number of 
kernels that are edible when the pot is left on the stove for a given number of \code{T} seconds.\cr}
%A short description of what the function(s) do(es) (one paragraph, a few lines only). (If a description is too long and cannot easily be shortened, the file probably tries to document too much at once.) This is mandatory except for package-overview files.

\usage{popcorn(T=120)} 
%One or more lines showing the synopsis of the function(s) and variables documented in the file. These are set in typewriter font. This is an R-like command.
% The usage information specified should match the function definition exactly (such that automatic checking for consistency between code and documentation is possible).

% A key:value list of input arguments.
\arguments{

\item{T}{
  the number of seconds that the pot is left on the stove. The default amount, if not provided, is 120 seconds.
}
}

% A detailed if possible precise description of the functionality provided, extending the basic information in the \description slot.
\details{This function simulates the number of edible popcorn kernels that are made when cooking a fixed number of kernels at the same heat setting 
for a given amount of \code{T} seconds.\cr
Time durations less than 77 seconds are not supported. A vector (list) of time values is not permitted, since the goal is to perform sequential 
experimentation to determine the optimum time, with the fewest number of function calls.
}

% Description of the function's return value.
\value{Returns the number of edible popcorn kernels. Random noise is added for some realism.}

% A section with references to the literature. Use \url{} or \href{}{} for web pointers.
\references{Please see Chapter 5 of the following book: Kevin Dunn, 2010 to 2015, \emph{Process Improvement using Data}, \url{http://learnche.org/pid}
}

% Use this for a special note you want to have pointed out. Multiple \note sections are allowed, but might be confusing to the end users.
%\note{}

\author{Kevin Dunn, <kevin.dunn@mcmaster.ca>}
%Information about the author(s) of the Rd file. Use \email{} without extra delimiters to specify email addresses, or \url{} or \href{}{} for web pointers.

\seealso{\code{\link{grocery}}, \code{\link{manufacture}}}
%Pointers to related R objects, using \code{\link{...}} to refer to them (\code is the correct markup for R object names, and \link produces hyperlinks in output formats which support this. See Marking text, and Cross-references).
\examples{
# Cooking for a very short duration is not supported. 
# For example, popcorn(T=50) will fail

# Cooking from 77 seconds onwards is supported
popcorn(T=120)

# What happens if we leave the pot on the stove for too long?
popcorn(T=500)

# Can you find the optimum time to cook on the stove 
# using the fewest number of function calls?
}

%Examples are not only useful for documentation purposes, but also provide test code used for diagnostic checking of R code. By default, text inside \examples{} will be displayed in the output of the help page and run by example() and by R CMD check. You can use \dontrun{} for text that should only be shown, but not run, and \dontshow{} for extra commands for testing that should not be shown to users, but will be run by example(). (Previously this was called \testonly, and that is still accepted.)

% Text inside \dontrun{} is 'verbatim', but the other parts of the \examples section are R-like text.

%For example,

%x <- runif(10)       # Shown and run.
%\dontrun{plot(x)}    # Only shown.
%\dontshow{log(x)}    # Only run.
%Thus, example code not included in \dontrun must be executable! In addition, it should not use any system-specific features or require special facilities (such as Internet access or write permission to specific directories). Text included in \dontrun is indicated by comments in the processed help files: it need not be valid R code but the escapes must still be used for %, \ and unpaired braces as in other verbatim text.

\concept{ design of experiments }
