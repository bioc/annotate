\name{readGEOAnn}
\alias{readGEOAnn}
\alias{readIDNAcc}
\title{Function to extract data from the GEO web site}
\description{
  Data files that are available at GEO web site are identified by GEO
  accession numbers. Given the url for the CGI script at GEO and
  a GEO accession number, the functions extract data from the web site
  and returns a matrix containing the data.
}
\usage{
readGEOAnn(url, GEOAccNum)
readIDNAcc(url, GEOAccNum)
}
\arguments{
  \item{url}{\code{url} the url for the CGI script at GEO}
  \item{GEOAccNum}{\code{GEOAccNum} a character string for the GEO
    accession number of a desired file (e. g. GPL97)}
}
\details{
  \code{url} is the CGI script that processes user's
  request. \code{\link{readGEOAnn}} invokes the CGI by passing a GEO 
  accession number and then processes the data file obtained.

  \code{\link{readIDNAcc}} calls \code{\link{readGEOAnn}} to read the
  data and the extracts the columns for probe ids and accession numbers.
  The \code{GEOAccNum} has to be the id for an Affymetrix chip.
}
\value{
  Both functions return a matrix.
}
\references{\url{www.ncbi.nlm.nih.gov/geo}}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{GEO-class}}}
\examples{
temp <- readGEOAnn(GEOAccNum = "GPL49")
temp2 <- readIDNAcc(GEOAccNum = "GPL97")
}
\keyword{manip}

