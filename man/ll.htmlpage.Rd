\name{ll.htmlpage}
\alias{ll.htmlpage}
\title{ Turn LocusLink ID's into a Clickable Web Page }
\description{
  Given a set of LocusLink accession numbers we create a web page with
  one element per gene that is clickable and produces the LL webpage for
  that gene.
}
\usage{
ll.htmlpage(genelist, filename, title, othernames)
)
}
\arguments{
  \item{genelist}{A character vector of gene names for LocusLink. }
  \item{filename}{The name of the file to store the HTML in. }
  \item{title}{A title for the web page}
  \item{othernames}{Other values to be included in the table}
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
\seealso{ \code{\link{read.annotation}}}

\examples{
 gnames <- as.character(6810:6820)
 ll.htmlpage(gnames, "fff.html")
 #to remove the file
 system("rm fff.html")
}
\keyword{ file }

