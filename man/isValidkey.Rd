\name{isValidKey}
\alias{isValidKey}
\title{Determine whether IDs are valid for a package.}
\description{
  Given a list of IDs and a package, determine whether or not these IDs
  are valid IDs for this package.
}
\usage{
isValidKey(ids, pkg)
}
\arguments{
  \item{ids}{A character vector containing IDs that you wish to validate.}
  \item{pkg}{The package name of the chip for which we wish to validate IDs.}
}
\details{
  Every package has some kind of ID that is central to that package.
  For chip-based packages this will be some kind of probe, and for the
  organism based packages it will be something else (usually an entrez
  gene ID).  This takes a list of IDs and tests to see whether or not
  they are present (valid) in a particular package.
}
\value{
  A vector of TRUE or FALSE values corresponding to whether or not the
  ID is valid.
}
\author{Marc Carlson}
\seealso{\code{\link{updateSymbolsToValidKeys}}}
\examples{
\dontrun{
  ## 2 bad IDs and a 3rd that will be valid
  ids <- c("15S_rRNA_2","21S_rRNA_4","15S_rRNA")
  isValidKey(ids, "org.Sc.sgd")

  ## 2 good IDs and a 3rd that will not be valid
  ids <- c("5600","7531", "altSymbol")
  isValidKey(ids, "org.Hs.eg")
}
}
\keyword{manip}
