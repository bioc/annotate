\name{getSYMBOL}
\alias{getSYMBOL}
\alias{getGO}
\alias{getGOdesc}
\alias{getPMID}
\alias{getLL}
\alias{contents}
\title{Functions to deal with Data Packages}
\description{
  The functions documented here are intended to make it easier to map
  from a set of manufacturers identifiers (such as you will get from the
  chips etc) to other identifiers.
}
\usage{
getSYMBOL(x, data = "hgu95av2")
getLL(x, data = "hgu95av2")
getGO(x, data = "hgu95av2")
getPMID(x, data = "hgu95av2")
getGOdesc(x, which=c("MF","BP","CC") )
contents(object)
}
\arguments{
  \item{x}{The identifiers to be mapped (usually manufacturer)}
  \item{data}{A character string identifying the data source }
  \item{which}{For \code{getGOdesc} indicates which GO hierarchy to use.}
  \item{object}{The environment (data table) that you want to get all
    contents from}
}
\details{
  Appropriate translations are done. In some cases such as \code{getLL}
  and \code{getSYMBOL} there will only be one match and a vector is
  returned. In other cases such as \code{getPMID} and \code{getGO} there
  may be multiple matches and a list is returned.
  For \code{getGOdesc} the input vector is GO identifiers (not
  manufacturer identifiers) and in the output any identifier that does
  not have a description in the selected GO hierarchy (MF, BP or CC) is
  dropped. 
}
\value{
  Either a vector or a list depending on whether multiple values per
  input are possible.
}
\author{R. Gentleman}
\seealso{\code{\link{multiget}}}
\examples{
  data(eset)
  gN <- geneNames(eset)[100:105]
  getSYMBOL(gN)
  gg<- getGO(gN)
  getGOdesc(gg[[2]], "MF")
  getLL(gN)
  getPMID(gN)
}
\keyword{manip}
