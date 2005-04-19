\name{PWAmat}
\alias{PWAmat}
\title{A function to compute the probe to KEGG pathway incidence matrix.}
\description{
  For a given chip we compute the mapping from probes to KEGG pathways.
}
\usage{
PWAmat(data)
}

\arguments{
  \item{data}{The name of the chip for which the incidence matrix should
  be computed.}
}
\details{
  Not much to say, just find which probes are in which pathways and
  return the incidence matrix, with pathways as rows and probes as
  columns. 
}
\value{
 A matrix containing zero or one, depending on whether the probe
  (column) is in a pathway (row).
}

\author{ R. Gentleman }

\seealso{ \code{\link{KEGG2heatmap}}, \code{\link{GOmnplot}} }
\examples{
  if(require("hgu95av2") ) {
    Am1 = PWAmat("hgu95av2")
 }
}
\keyword{manip}

