\name{hgu95Achrom}
\non_function{}
\docType{data}
\alias{hgu95Achrom}
\alias{hgu95AchromNames}

\title{Annotation data for the Affymetrix HGU95A GeneChip}

\usage{data(hgu95Achrom)
 data(hgu95AchromNames)
}

\format{
  This data set provides an environment (treat as a hashtable)
  with mappings from the Affymetrix
  identifiers to chromosome number/name.
  The environment functions like a hashtable and can be accessed using
  \code{multiget}.
  If the returned value is \code{NA} then the current 
  mapping was unable to identify this. Mappings and data sources are
  constantly evolving so updating often is recommended.

  \code{hgu95AchromNames} is a character vector with the chromosome
  names in the order, 1 through 22 then X then Y. This is useful
  for ordering the chromosomes in some plotting functions.
}
\description{Data, in the form of environments for the Affymetrix U95A
 chip.}

\source{The \code{AnnBuilder} package.}

\examples{
 data(hgu95Achrom)
 data(eset)
 multiget(geneNames(eset)[330:340], env=hgu95Achrom)
 data(hgu95AchromNames)
}
\keyword{datasets}
