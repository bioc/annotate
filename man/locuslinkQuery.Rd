\name{locuslinkQuery}
\alias{locuslinkQuery}
\title{A function to open the browser to Locuslink using a provided
  search query. }
\description{Given a search string, will query the Locuslink site and
  passes the returned page to the browser.}
\usage{
locuslinkQuery(query,...,lladdress="LocusLink/", brose=TRUE)
}
\arguments{
  \item{query}{The search query to use}
  \item{...}{The species to search on.  Defaults to "Hs" - human.}
  \item{lladdress}{The subdirectory off of the NCBI site for
    LocusLink. }
  \item{browse}{If TRUE, will open the URL in a web browser}

}
\details{
  A simple function which takes a search string and a set of species,
  and will perform a locuslink query, returning the results to the
  browser.  Tips on creating good search strings, as well as current
  species abbreviations can be found at: \code{http://www.ncbi.nih.gov/LocusLink/help.html}
}
\value{
  The URL found is returned.  If \code{browse} is TRUE, the user's web
  browser is directed to that URL.

}
\author{Jeff Gentry }
\seealso{\code{\link{genbank}} \code{\link{pubmed}} \code{\link{locuslinkByID}}}
\examples{
   if( interactive() )
      locuslinkQuery("abc","Hs","Mm")
}
\keyword{ interface }
