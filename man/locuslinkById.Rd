\name{locuslinkByID}
\alias{locuslinkByID}

\title{A function to open the browser to Locuslink with the selected gene. }
\description{
  Given one or more Locuslink accession numbers the browser is given a
  URL to query for that \em{gene}. 
}
\usage{
locuslink(..., lladdress="LocusLink/")
}
\arguments{
  \item{...}{The Locuslink accession numbers. }
  \item{lladdress}{The subdirectory off of the NCBI site for LocusLink. }
}
\details{
  A simple function to open the appropriate web page in the users
  browser.  If the user supplies more then one accession number, the
  other results will be accessible through the LocusLink page under the
  "View" pulldown menu on the page.
}
\value{
No value is returned. The users web browser is directed to the
appropriate link.
}

\author{Jeff Gentry }

\seealso{\code{\link{genbank}} \code{\link{pubmed}} \code{\link{locuslinkQuery}}}

\examples{
  locuslinkByID(7531,9025,2315)
}
\keyword{ interface }
