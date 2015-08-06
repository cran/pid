% http://cran.r-project.org/doc/manuals/r-release/R-exts.html

\name{tradeOffTable}
%name typically is the basename of the Rd file containing the documentation. It is the "name" of the Rd object represented by the file and has to be unique in a package. To avoid problems with indexing the package manual, it may not contain `, | nor @ , and to avoid possible problems with the HTML help system it should not contain '/' nor a space. (LaTeX special characters are allowed, but may not be collated correctly in the index.) There can only be one \name entry in a file, and it must not contain any markup. Entries in the package manual will be in alphabetic order of the \name entries.

\alias{tradeOffTable}
%The \alias sections specify all "topics" the file documents. This information is collected into index data bases for lookup by the on-line (plain text and HTML) help systems. The topic can contain spaces, but (for historical reasons) leading and trailing spaces will be stripped. Percent and left brace need to be escaped by a backslash.


\title{A trade-off table of fractional factorial designs}
%Title information for the Rd file. This should be capitalized and not end in a period; try to limit its length to at most 65 characters for widest compatibility.
% There must be one (and only one) \title section in a help file.

\description{Creates a new plot that shows a trade-off table for fractional factorial designs.}
%A short description of what the function(s) do(es) (one paragraph, a few lines only). (If a description is too long and cannot easily be shortened, the file probably tries to document too much at once.) This is mandatory except for package-overview files.

\usage{tradeOffTable()} 
%One or more lines showing the synopsis of the function(s) and variables documented in the file. These are set in typewriter font. This is an R-like command.
% The usage information specified should match the function definition exactly (such that automatic checking for consistency between code and documentation is possible).

% A key:value list of input arguments.
% \arguments{}


% A detailed if possible precise description of the functionality provided, extending the basic information in the \description slot.
\details{Displays the following trade-off table:

	\if{html}{\figure{trade-off-table.png}{options: width="100\%" alt="Figure: DOE-trade-off-table.png"}}
    \if{latex}{\figure{trade-off-table.pdf}{options: width=7cm}}
	
	The rows in the table are the number of experiments done in the fractional factorial (\eqn{n}).\cr
	The columns are the number of factors under investigation in the design (\eqn{k}).\cr
	The cell at a particular row/column intersection gives several pieces of information:
	\itemize{
			\item	The top-left entry of the form: \eqn{2^{k-p}=n}. For example, \eqn{p=1} corresponds to half-fractions, 
			and \eqn{p=2} corresponds to quarter-fractions.
			\item	The subscript in the top-left entry, written in Roman numerals gives the design resolution. For example, \eqn{IV} 
			corresponds to a resolution 4 design, implying 2-factor interactions are at most confounded with other 2-factor interactions.
			\item	The bold entries in the bottom-right tell how to generate the remaining factors in the design. \cr
					A "full" entry indicates a full factorial; while "twice" indicates a twice replicated full factorial.
	}
	Blank entries are impossible fractional factorial combinations.
	
	A detailed explanation of the table is provided in the book reference.
}

% Description of the function's return value.
\value{Create a new plot displaying the trade-off table.}

% A section with references to the literature. Use \url{} or \href{}{} for web pointers.
\references{Chapter 5 of the following book: Kevin Dunn, 2010 to 2015, \emph{Process Improvement using Data}, \url{http://learnche.org/pid}

Please see this paper to gain an understanding of how these trade-off tables are constructed:\cr
Arthur Fries and William G. Hunter, (1980) Minimum Aberration \eqn{2^{k-p}} Designs, \emph{Technometrics}, \bold{22}(4), pp. 601-608, \url{https://www.jstor.org/stable/1268198}
}

% Use this for a special note you want to have pointed out. Multiple \note sections are allowed, but might be confusing to the end users.
%\note{}

\author{Kevin Dunn, <kevin.dunn@mcmaster.ca>}
%Information about the author(s) of the Rd file. Use \email{} without extra delimiters to specify email addresses, or \url{} or \href{}{} for web pointers.

\seealso{\code{\link{tradeoff}} which can be used to extend the table out to more factors or more experiments.} 
%Pointers to related R objects, using \code{\link{...}} to refer to them (\code is the correct markup for R object names, and \link produces hyperlinks in output formats which support this. See Marking text, and Cross-references).
\examples{
tradeOffTable()
}

%Examples are not only useful for documentation purposes, but also provide test code used for diagnostic checking of R code. By default, text inside \examples{} will be displayed in the output of the help page and run by example() and by R CMD check. You can use \dontrun{} for text that should only be shown, but not run, and \dontshow{} for extra commands for testing that should not be shown to users, but will be run by example(). (Previously this was called \testonly, and that is still accepted.)

% Text inside \dontrun{} is 'verbatim', but the other parts of the \examples section are R-like text.

%For example,

%x <- runif(10)       # Shown and run.
%\dontrun{plot(x)}    # Only shown.
%\dontshow{log(x)}    # Only run.
%Thus, example code not included in \dontrun must be executable! In addition, it should not use any system-specific features or require special facilities (such as Internet access or write permission to specific directories). Text included in \dontrun is indicated by comments in the processed help files: it need not be valid R code but the escapes must still be used for %, \ and unpaired braces as in other verbatim text.

\concept{ design of experiments }
