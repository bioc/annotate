\name{accessionToUID}
\alias{accessionToUID}
\title{A function to convert accession values to NCBI UIDs.}
\description{
  Given one or more accession values, this function will attempt to
  convert them into NCBI UID values.
}
\usage{
accessionToUID(...,db=c("genbank","pubmed")[1])
}
\arguments{
  \item{...}{Accession numbers to be transformed.}
  \item{db}{Which database this accession number refers to, defaults to Genbank}
}
\details{
  Utilizes the PubMed tool pmqty.cgi to convert an accession number
  into a valid NCBI UID number.

  WARNING:  The powers that be at NCBI have been known to ban the IP
  addresses of users who abuse their servers (currently defined as less
  then 2 seconds between queries).  Do NOT put this function in a type
  loop or you may find your access revoked.
}
\value{
  Returns either a valid NCBI UID value or NULL (if there was nothing
  available).
}
\author{Jeff Gentry}
\seealso{\code{\link{pubmed}}, \code{\link{locuslinkByID}},
  \code{\link{locuslinkQuery}}, \code{\link{xmlTreeParse}}}
\examples{

     ## The two returns from genbank should be the same
     genbank("U03397",type="accession",disp="data")
     x <- accessionToUID("U03397",db="genbank")
     genbank(x, type="uid",disp="data")

     ## Can handle multiple inputs
     accessionToUID("M16653","U892893",db="pubmed")
}
\keyword{interface}
