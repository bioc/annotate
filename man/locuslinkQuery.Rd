\name{locuslinkQuery}
\alias{locuslinkQuery}
\title{A function to open the browser to Locuslink using a provided
  search query. }
\description{Given a search string, will query the Locuslink site and
  passes the returned page to the browser.}
\usage{
locuslinkQuery(query,...,lladdress="LocusLink/")
}
\arguments{
  \item{query}{The search query to use}
  \item{...}{The species to search on.  Defaults to "Hs" - human.}
  \item{lladdress}{The subdirectory off of the NCBI site for LocusLink. }
}
\details{
  A simple function which takes a search string and a set of species,
  and will perform a locuslink query, returning the results to the
  browser.  Tips on creating good search strings, as well as current
  species abbreviations can be found at: \code{http://www.ncbi.nih.gov/LocusLink/help.html}
}
\value{
  No value is returned.  The user's web browser is directed to the
  appropriate link.
}
\author{Jeff Gentry }
\seealso{\code{\link{genbank}} \code{\link{pubmed}} \code{\link{locuslinkByID}}}
\examples{
   locuslinkQuery("abc","Hs","Mm")
}
\keyword{ interface }
