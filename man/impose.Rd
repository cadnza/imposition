\name{impose}
\alias{impose}
\alias{imposition}
\title{Imposition - Bookbinding}
\description{
\code{impose} is a niche function that takes a number of pages and a number of sheets per signature and returns the order in which those pages should be printed as a vector along with some other info. Very handy for bookbinding.

Note that this function assumes you want \bold{two-sided booklet imposition}.

A few handy terms:

\itemize{
\item{
\bold{Signature}: A single booklet of which many are bound together to make a book
}
\item{
\bold{Sheet}: A single piece of paper that gets folded once for a signature
}
\item{
\bold{Leaf}: A single piece of paper as it appears in a book, front and back
}
\item{
\bold{Page}: A single side of a leaf
}
}

So in terms of printing (with two-sided sheets in booklet form), a leaf contains two pages, and a sheet contains four. So a signature made up of four sheets yields 16 pages.
}
\usage{
impose(
	nPageMinimum,
	nSheetsPerSignature
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nPageMinimum}{
  The number of pages you're planning to print. It's called \code{nPageMinimum} instead of \code{nPage} because the imposition process usually involves appending extra pages to the end of the document in order to make the total page count a multiple of the number of pages per signature.
	}
	\item{nSheetsPerSignature}{
	The number of sheets you'd like to stack into each signature. This will depend on your binding technique and the thickness of your paper, but 3-5 is standard.
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{impose} returns a list with three values:
\itemize{
\item{
\code{$order}: An integer vector representing the order in which to print pages to sheets, from left to right and then front to back. Each integer in the vector is a page number, and \code{NA} represents a blank page. Extra pages added during the imposition process are numbered according to the ordinary sequence, so if your book only has 169 pages but you see a Page 172 in \code{$order}, it means that you'll have a few extra pages when you do your binding. This is normal. Most publishers just leave them blank.
}
\item{
\code{$extraPages}:
The number of extra pages that were added. Again, when performing imposition, the final page number has to be a multiple of the number of pages per signature.
}
\item{
\code{$signatures}:
The number of signatures required to bind your book.
}
}
}
\references{
\href{https://en.wikipedia.org/wiki/Imposition}{Imposition - Wikipedia}
}
\author{
\href{https://github.com/cadnza}{Jon Dayley}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
impose(
	nPageMinimum=169,
	nSheetsPerSignature=4
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
