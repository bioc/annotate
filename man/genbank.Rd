\name{genbank}
\alias{genbank}
\title{A function to open the browser to Genbank with the selected gene. }
\description{
 Given a Genbank accession number the browser is given a URL to query
 for that  \em{gene}. 
}
\usage{
genbank(geneid, gbaddress)
}
\arguments{
  \item{geneid}{The Genbank accession number. }
  \item{gbaddress}{The URL for Genbank }
}
\details{
  A simple query in the form required by Genbank is constructed and sent
  to the users browser. If a browser is running it is used, otherwise
  one is started.
}
\value{
No value is returned. The users web browser is directed to the
appropriate link.
}

\author{R. Gentleman }

\seealso{\code{\link{locuslink}}}

\examples{
 genbank("X07820")
}
\keyword{interface }
