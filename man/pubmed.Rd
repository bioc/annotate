\name{pubmed}
\alias{pubmed}

\title{A function to open the browser to Pubmed with the selected gene. }
\description{
 Given a Pubmed identifier the browser is given a URL to query
 for that citation. 
}
\usage{
pubmed(pmid, pmaddress)
}
\arguments{
  \item{pmid}{The Pubmed identifier. }
  \item{pmaddress}{The URL for Pubmed }
}
\details{
  A simple function to open the appropriate web page in the users browser.
}
\value{
No value is returned. The users web browser is directed to the
appropriate link.
}

\author{R. Gentleman }

\seealso{\code{\link{genbank}}, \code{\link{locuslink}} }

\examples{
  pubmed("11780146") #Structure of the Bcr-Abl 
}
\keyword{ interface }
