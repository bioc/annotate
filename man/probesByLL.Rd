\name{probesByLL}
\alias{probesByLL}

\title{A function that groups probe ids by LocusLink ids}
\description{
  This  function takes the name of a platform specific annotation data
  package (e. g. an Affymetrix chip) and returns a list of vectors of
  character string with LocusLink ids as the names of the lists of
  vectors and probe ids corresponding to the LocusLink ids as values.
}
\usage{
probesByLL(baseName)
}

\arguments{
  \item{baseName}{\code{baseName} a character string for the name of
    platform specific annotation data package}
}
\details{
  Each platform specific annotation data package has a LOCUSID
  environment that maps probe ids to LocusLink ids. This function does
  reverse mappings by grouping probe ids under unique LocusLink ids and
  returns the mappings as a list.
}
\value{
  This function returns a list of vectors of character strings with
  unique LocusLink ids as the names and probe ids corresponding to the
  LocsuLink ids as values.
}

\author{Jianhua Zhang}

\examples{
    if(require("hgu95av2")){
        tt <- probesByLL("hgu95av2")
        tt[1:4]
    }
}
\keyword{ manip }
