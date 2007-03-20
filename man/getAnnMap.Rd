\name{getAnnMap}
\alias{getAnnMap}

\title{Get annotation map}
\description{
  This function retrieves a map object from an annotation data
  package.  It is intended to serve as a common interface for
  obtaining map objects from both SQLite-based and environment-based
  annotation data packages.
}
\usage{
getAnnMap(map, chip, load = FALSE, type = c("db", "env"))
}

\arguments{
  \item{map}{a string specifying the name of the map to retrieve.  For
      example, \code{"ENTREZID"} or \code{"GO"}}
  \item{chip}{a string describing the chip or genome}
  \item{load}{a logical value.  When \code{TRUE}, \code{getAnnMap}
      will try to load the annotation data package if it is not
      already attached.}
  \item{type}{a character vector of one or more annotation data
      package types.  The currently supported types are \code{"db"}
      and \code{"env"}.  If \code{load} is \code{TRUE}, you can specify both \code{"db"}
      and \code{"env"} and the order will determine which type is
      tried first.  This provides a fall-back mechanism when the
      preferred annotation data package type is not available.}
}
\details{
  \code{getAnnMap} expects the annotation data package to be loaded
  and attached.  If \code{load} is TRUE, the package will be attached.
}
\value{
  If \code{type} is \code{"db"}, an S4 object representing the
  requested map.  If \code{type} is \code{"env"}, an R
  \code{environment} object representing the requested map.
}

\author{Seth Falcon}

\examples{
map <- getAnnMap("ENTREZID", "hgu95av2", load=TRUE, type=c("env", "db"))
class(map)
}

\keyword{manip}
