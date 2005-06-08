\name{htmlpage}
\alias{htmlpage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions to build HTML pages }
\description{
 This function is a modification of \code{ll.htmlpage},
 designed to be able to create links for more than one set of ids.
}
\usage{
htmlpage(genelist, filename, title, othernames, table.head, table.center = TRUE, repository = "ll")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genelist}{ A list or \code{data.frame} of character vectors
    containing ids to be made into hypertext links. See details for more
    information.}
  \item{filename}{ A filename for the resultant HTML table. }
  \item{title}{ A title for the table. }
  \item{othernames}{ A list or \code{data.frame} of other things to add
    to the table. These will not be hyperlinks. The list of othernames
    can contain vectors, matrices, \code{data.frames} or lists. }
  \item{table.head}{ A character vector of column
    headers for the table.}
  \item{table.center}{ Center the table? Defaults to \code{TRUE}. }
  \item{repository}{ A list of repositories to use
    for creating the hypertext links. See details for more information.}
}
\details{
 This is a modification of \code{ll.htmlpage}, which is
 limited to a single column of hyperlinked ids and lists of vectors for
 othernames.
 This function will accept a list or \code{data.frame} of character
 vectors, each containing different ids that are to be turned into
 hyperlinks (e.g., a list containing affy ids, genbank accession
 numbers, and locus link ids). For instances where there are more than
 one id per gene, use a sub-list of character vectors. See the vignette
 'prettyOutput' for more information. Othernames should be a list or
 \code{data.frame}. Again, if there are multiple entries for a given
 gene, use a sub-list.
}
\value{
  This function is used only for the side effect of creating an HTML table.
}

\author{Robert Gentleman <rgentlem@jimmy.harvard.edu>, further
  modifications by James W. MacDonald <jmacdon@med.umich.edu> }

\examples{
library(annotate)
gnames <- as.character(6810:6820)
htmlpage(gnames, "fff.html")
file.remove("fff.html")
}
\keyword{ manip }% at least one, from doc/KEYWORDS
