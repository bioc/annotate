\name{getSYMBOL}
\alias{getSYMBOL}
\alias{getGO}
\alias{getGOdesc}
\alias{getPMID}
\alias{getLL}
\alias{contents}
\alias{lookUp}
\alias{getUniqAnnItem}
\alias{contents,environment-method}
\title{Functions to deal with Data Packages}
\description{
  The functions documented here are intended to make it easier to map
  from a set of manufacturers identifiers (such as you will get from the
  chips etc) to other identifiers.
}
\usage{
getSYMBOL(x, data)
getLL(x, data)
getGO(x, data)
getPMID(x, data)
getGOdesc(x, which=c("MF","BP","CC") )
contents(object)
lookUp(x, data, what)
getUniqAnnItem()
}
\arguments{
  \item{x}{The identifiers to be mapped (usually manufacturer)}
  \item{data}{A character string identifying the data source }
  \item{which}{For \code{getGOdesc} indicates which GO hierarchy to use.}
  \item{object}{The environment (data table) that you want to get all
    contents from}
  \item{what}{\code{what} a character string for the name of an
    annotation element of an annotation data package}
}
\details{
  Appropriate translations are done. In some cases such as \code{getLL}
  and \code{getSYMBOL} there will only be one match and a vector is
  returned. In other cases such as \code{getPMID} and \code{getGO} there
  may be multiple matches and a list is returned.
  For \code{getGOdesc} the input vector is GO identifiers (not
  manufacturer identifiers) and in the output any identifier that does
  not have a description in the selected GO hierarchy (MF, BP or CC) is
  dropped. \code{lookUp} is a general function that can be used to look
  up matches. All other translation functions use \code{lookUp}

  \code{getUniqAnnItem} keeps track of the annotation elements that have
  one to one mappings. \code{lookUp} returns a named vector for these
  elements but a list of named vectors for others.

  A BioC annotation data package contains annotation data environments
  whose names are pkckage name (e. g. hgu95av2) + element name
  (e. g. PMID). \code{what} must be one of the element names for the
  given data package.  
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
   ## A bit of a hack to not have a package dependency on hgu95av2
   ## but need to fiddle w/ the warn level to not fail the example anyways.
   curWarn <- getOption("warn")
   options(warn=0)
   on.exit(options(warn=curWarn), add=TRUE)
  if( require("hgu95av2", character.only=TRUE)
       && require("GO", character.only=TRUE) ) {
      lookUp(gN, "hgu95av2", "SYMBOL")
      # Same as lookUp for SYMBOL
      getSYMBOL(gN,"hgu95av2" )
      gg<- getGO(gN, "hgu95av2")
      lookUp(gg[[2]], "GO", "MFID2TERM")
      # Same as lookUp for MFID2TERM
      getGOdesc(gg[[2]], "MF")
      getLL(gN, "hgu95av2")
      getPMID(gN, "hgu95av2")
  } else
   cat("could not run all examples\n")
}
\keyword{manip}
