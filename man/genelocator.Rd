\name{genelocator}
\alias{genelocator}
\title{A function to identify genes by their LocusLink (or other id).  }
\description{
   This function uses \code{locator} to provide some interaction with an
   \code{image} plot of genetic data. It is currently not implemented
   except in skeleton form.
}
\usage{
genelocator(x)
}
\arguments{
  \item{x}{Undeterimined }
}
\details{
}
\value{
  This function is executed mainly for its side effect. When an image
  plot is active then \code{genelocator} can be called. When the image
  is clicked on, using the left mouse button, the users web browser is
  activated and a web page (determined by one of the arguments to
  \code{genelocator}) will be displayed.
}

\author{ R. Gentleman }
\note{ }

\seealso{ \code{\link{locator}}}

\examples{
## This is an interactive function so the examples won't work!

}
\keyword{ iplot }
