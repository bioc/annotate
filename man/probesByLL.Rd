\name{probesByLL}
\alias{probesByLL}

\title{A function that groups probe ids in a given chip by LocusLink ids}
\description{
  This  function takes a name for an annotation data package for a given
  chip and returns a list of vector of character string with LocusLink
  ids as the name of the list and vectors of probe ids correspinding to
  the LocusLink ids as values.
}
\usage{
probesByLL(chipName)
}

\arguments{
  \item{chipName}{\code{chipName} a character string for the name of the
  annotation data package for an Affymetrix chip}
}
\details{
  Each annotation package for Affymetrix chips has an LOCUSID
  environment that mappes probe ids to LocusLink ids. This function does
  reverse mappings by grouping probe ids under unique LocusLink ids and
  returns the mappings as list.
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
