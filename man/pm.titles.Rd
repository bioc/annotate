\name{pm.titles}
\alias{pm.titles}
\title{Obtain the titles of the PubMed abstracts.  }
\description{
 This function returns the titles from a list of PubMed abstracts.
}
\usage{
pm.titles(absts)
}
\arguments{
  \item{absts}{The list of PubMed abstracts. }
}
\details{
  It simply uses \code{sapply}
}
\value{
A character vector of length equal to the number of abstracts. Each
element is the title of the corresponding abstract.
}
\author{ Robert Gentleman }

\seealso{ \code{\link{pm.abstGrep}} }

\examples{
 hoxa9 <- "37809_at"
   ## A bit of a hack to not have a package dependency on hgu95av2
   ## but need to fiddle w/ the warn level to not fail the example anyways.
   curWarn <- getOption("warn")
   options(warn=0)
   on.exit(options(warn=curWarn), add=TRUE)

 if( require(hgu95av2) ) {
    absts <- pm.getabst(hoxa9, "hgu95av2")
    pm.titles(absts)
 }
}
\keyword{ manip }
