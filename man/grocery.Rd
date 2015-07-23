\encoding{UTF-8}
\name{grocery}
%name typically is the basename of the Rd file containing the documentation. It is the "name" of the Rd object represented by the file and has to be unique in a package. To avoid problems with indexing the package manual, it may not contain `, | nor @ , and to avoid possible problems with the HTML help system it should not contain '/' nor a space. (LaTeX special characters are allowed, but may not be collated correctly in the index.) There can only be one \name entry in a file, and it must not contain any markup. Entries in the package manual will be in alphabetic order of the \name entries.

\alias{grocery}
%The \alias sections specify all "topics" the file documents. This information is collected into index data bases for lookup by the on-line (plain text and HTML) help systems. The topic can contain spaces, but (for historical reasons) leading and trailing spaces will be stripped. Percent and left brace need to be escaped by a backslash.

\title{Simulation of grocery store profits for a single product}
%Title information for the Rd file. This should be capitalized and not end in a period; try to limit its length to at most 65 characters for widest compatibility. There must be one (and only one) \title section in a help file.

\description{The hourly profit made when selling the product at price \code{P} and the product is displayed at height \code{H} [cm] on the shelf.}
%A short description of what the function(s) do(es) (one paragraph, a few lines only). (If a description is too long and cannot easily be shortened, the file probably tries to document too much at once.) This is mandatory except for package-overview files.

\usage{grocery(P=3.46, H=150)} 
%One or more lines showing the synopsis of the function(s) and variables documented in the file. These are set in typewriter font. This is an R-like command.
% The usage information specified should match the function definition exactly (such that automatic checking for consistency between code and documentation is possible).

% A key:value list of input arguments.
\arguments{

\item{P}{
  the selling price of the product. The default amount, if unspecified, is €3.46.
}
\item{H}{
  the display height of the product, as measured from the ground up. The default value, if unspecified, is 150cm.
}
}

% A detailed if possible precise description of the functionality provided, extending the basic information in the \description slot.
\details{This function simulates the hourly profit in a grocery store of selling a particular product.\cr
Two factors can be adjusted by the user to determine the optimum:
\itemize{
	\item   \code{P}: The selling price of the product (must be a positive value).
	\item   \code{H}: The height that the product is displayed at (must be a positive value).
}

Can you determine the best combination of conditions, using a systematic method (i.e. not by trial and error)?
}

% Description of the function's return value.
\value{Returns the hourly profit made. Random noise is added for some realism.}

% A section with references to the literature. Use \url{} or \href{}{} for web pointers.
\references{Please see Chapter 5 of the following book: Kevin Dunn, 2010 to 2015, \emph{Process Improvement using Data}, \url{http://learnche.org/pid}
}

% Use this for a special note you want to have pointed out. Multiple \note sections are allowed, but might be confusing to the end users.
%\note{}

\author{Kevin Dunn, <kevin.dunn@mcmaster.ca>}
%Information about the author(s) of the Rd file. Use \email{} without extra delimiters to specify email addresses, or \url{} or \href{}{} for web pointers.

\seealso{\code{\link{popcorn}}, \code{\link{manufacture}}}
%Pointers to related R objects, using \code{\link{...}} to refer to them (\code is the correct markup for R object names, and \link produces hyperlinks in output formats which support this. See Marking text, and Cross-references).
\examples{
# Selling at the default settings of price (€3.46) 
# and shelf height (150cm):
grocery()

# Let's try selling for a low price, €2.75, 
# and put the product 120 cm off the ground
grocery(P=2.75, H=120)

# What happens if the product is too high 
# for the average person?
grocery(P=1.25, H=200)

# Can you find the optimum combination of settings to 
# maximize the profit, but using only a few experiments?
}

%Examples are not only useful for documentation purposes, but also provide test code used for diagnostic checking of R code. By default, text inside \examples{} will be displayed in the output of the help page and run by example() and by R CMD check. You can use \dontrun{} for text that should only be shown, but not run, and \dontshow{} for extra commands for testing that should not be shown to users, but will be run by example(). (Previously this was called \testonly, and that is still accepted.)

% Text inside \dontrun{} is 'verbatim', but the other parts of the \examples section are R-like text.

%For example,

%x <- runif(10)       # Shown and run.
%\dontrun{plot(x)}    # Only shown.
%\dontshow{log(x)}    # Only run.
%Thus, example code not included in \dontrun must be executable! In addition, it should not use any system-specific features or require special facilities (such as Internet access or write permission to specific directories). Text included in \dontrun is indicated by comments in the processed help files: it need not be valid R code but the escapes must still be used for %, \ and unpaired braces as in other verbatim text.

\concept{ design of experiments }
