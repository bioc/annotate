\name{ll.htmlpage}
\alias{ll.htmlpage}
\title{ Turn LocusLink ID's into a Clickable Web Page }
\description{
  Given a set of LocusLink accession numbers we create a web page with
  one element per gene that is clickable and produces the LL webpage for
  that gene.
}
\usage{
ll.htmlpage(genelist, filename, title, othernames, table.head,
     table.center = TRUE, repository = "ll" )
)
}
\arguments{
  \item{genelist}{A character vector of gene names for LocusLink. }
  \item{filename}{The name of the file to store the HTML in. }
  \item{title}{A title for the web page}
  \item{othernames}{Other values to be included in the table}
  \item{table.head}{A character vector of column labels for the table}
  \item{table.center}{A logical indicating whether the table should be
    centered}
  \item{repository}{A character string for the name of a public
    repositories. Valid values include "ll", "ug"} 
}
\details{
  A simple markup is used to provide clickable entries.
  Anyone that is energetic could greatly improve this.
}
\value{
  No value is returned. The function is evaluated solely for the side
  effect of producing the file \code{filename}.
}

\author{R. Gentleman }

\examples{
 gnames <- as.character(6810:6820)
 ll.htmlpage(gnames, "fff.html")

 file.remove("fff.html")
}
\keyword{ file }

