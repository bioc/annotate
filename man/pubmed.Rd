\name{pubmed}
\alias{pubmed}

\title{A function to open the browser to Pubmed with the selected gene. }
\description{
  Given a vector of Pubmed identifiers, the user can either have a
  browser display a URL showing a Pubmed query for those identifiers,
  or a XMLdoc object with the same data.
}
\usage{
pubmed(...,disp=c("data","browser")[1], pmaddress=.pmfetch())
}
\arguments{
  \item{...}{Vectorized set of Pubmed ID's}
  \item{disp}{Either "Data" or "Browser" (default is data).  Data
    returns a XMLDoc, while Browser will display information in the
    user's browser.}
  \item{pmaddress}{Specific path to the pubmed pmfetch engine from the
    NCBI website.}
}
\details{
  A simple function to retrieve Pubmed data given a specific ID, either
  through XML or through a web browser.  WARNING:  The powers that be
  at NCBI have been known to ban the IP addresses of users who abuse
  their servers (currently defined as less then 2 seconds between
  queries).  Do NOT put this function in a type loop or you may find
  your access revoked.
}
\value{
  If the option "data" is used, an object of type XMLDoc is returned.
  Otherwise, nothing is returned.
}

\author{R. Gentleman }

\seealso{\code{\link{genbank}}, \code{\link{locuslinkByID}},
  \code{\link{loucslinkQuery}}, \code{\link{xmlTreeParse}}}

\examples{
   for (dp in c("data","browser"))
     pubmed("11780146","11886385","11884611",disp=dp)
}
\keyword{ interface }
