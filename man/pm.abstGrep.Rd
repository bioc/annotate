\name{pm.abstGrep}
\alias{pm.abstGrep}
\title{An interface to grep for PubMed abstracts.  }
\description{
  A user friendly interface to the functionality provided by
 \code{pubmed}. 
}
\usage{
pm.abstGrep(pattern, absts, ...)
}
\arguments{
  \item{pattern}{A pattern for the call to \code{grep}. }
  \item{absts}{A list containing abstracts downloaded using \code{pubmed} or
  equivalent. }
  \item{\dots}{Extra arguments passed to \code{grep}. }
}
\details{
  The \code{absts} are a list of PubMed XML objects that have been downloaded 
  and parsed. This function lets the user quickly search the abstracts
  for any regular expression. The returned value is a logical vector
  indicating which of the abstracts contain the regular expression.
}
\value{
 The returned value is a logical vector
  indicating which of the abstracts contain the regular expression.
}
\author{ Robert Gentleman }

\seealso{ \code{\link{pm.getabst}}, \code{\link{pm.titles}}}

\examples{
  hoxa9 <- "37809_at"

   ## A bit of a hack to not have a package dependency on hgu95av2
   ## but need to fiddle w/ the warn level to not fail the example anyways.
   curWarn <- getOption("warn")
   options(warn=0)
   on.exit(options(warn=curWarn), add=TRUE)
  if( require(hgu95av2) ) {
     absts <- pm.getabst(hoxa9, "hgu95av2")

     pm.abstGrep("NUP98", absts[[1]])

     pm.abstGrep("apoptosis", absts[[1]])
  }

}
\keyword{ manip }
