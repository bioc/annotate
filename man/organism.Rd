\name{organism}
\alias{organism}
\alias{organism,character-method}
\title{Convenience function for getting the organism from an object or package}

\description{
  The most basic organism method just takes a character string (which
  represents a particular annotation package) and returns the organism
  that said package is based upon.
}

\usage{
  organism(object)
}

\arguments{
  \item{object}{a character string that names a package}
}

\value{
  The name of the organism used for this package
}
  

\author{Marc Carlson}

\examples{
  require(hgu95av2.db)
  organism("hgu95av2")
}

\keyword{manip}

