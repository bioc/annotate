\name{hgu95All}
\non_function{}
\docType{data}
\alias{hgu95All}

\title{Annotation data for the Affymetrix HGU95A GeneChip}

\usage{data(hgu95All)}
\description{Data, in the form of environments for the Affymetrix U95A
 chip.}


\format{
  These data sets provide environments with mappings from the Affymetrix
  identifiers to LocusLink identifiers.
  The environment functions like a hashtable and can be accessed using
  \code{multiget}.
  If the returned value is \code{NA} then the current 
  mapping was unable to identify this. Mappings and data sources are
  constantly evolving so updating often is recommended.
}

\source{The \code{AnnBuilder} package.}

\examples{
 data(hgu95All)
 data(eset)
 multiget(geneNames(eset)[330:340], env=hgu95All)

}
\keyword{datasets}
