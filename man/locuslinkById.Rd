\name{locuslinkByID}
\alias{locuslinkByID}

\title{A function to open the browser to Locuslink with the selected gene. }
\description{
  Given one or more Locuslink accession numbers the browser is given a
  URL to query for that \emph{gene}. 
}
\usage{
locuslinkByID(..., lladdress="LocusLink/", browse=TRUE)
}
\arguments{
  \item{...}{The LocusLink accession numbers. }
  \item{lladdress}{The subdirectory off of the NCBI site for LocusLink. }
  \item{browse}{If \code{TRUE}, will open the URL in a web browser}
}
\details{
  A simple function to open the appropriate web page in the users
  browser.  If the user supplies more then one accession number, the
  other results will be accessible through the LocusLink page under the
  "View" pulldown menu on the page.
}
\value{
  If \code{browse} is \code{TRUE}, the user's web browser is directed to
  that URL. The URL is returned.
}

\author{Jeff Gentry }

\seealso{\code{\link{genbank}} \code{\link{pubmed}}
  \code{\link{locuslinkQuery}}}

\examples{
  if( interactive() )
      locuslinkByID(7531,9025,2315)
}
\keyword{ interface }
