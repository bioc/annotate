\name{pm.getabst}
\alias{pm.getabst}
\title{ Obtain the abstracts for a set PubMed list. }
\description{
 The data provided by PubMed is reduced to a small set. This set is 
then suitable for further rendering.
}
\usage{
pm.getabst(geneids, basename)
}
\arguments{
  \item{geneids}{The identifiers used to find Abstracts }
  \item{basename}{The base name of the annotation package to use. }
}
\details{
We rely on the annotation in the package associated with the 
\code{basename} to provide PubMed identifiers for the genes described by
the gene identifiers.
With these in hand we then use the \code{pmfetch} utility to download the
PubMed abstracts in XML form. These are then translated (transformed) to a 
shorter version containing a small subset of the data provided by PubMed.
}
\value{
 A list containing one XML component for each abstract obtained.
}
\author{ Robert Gentleman }

\seealso{ \code{\link{pm.abstGrep}}, \code{\link{pm.titles}} }

\examples{
hoxa9 <- "U82759_at"

absts <- pm.getabst(hoxa9, "hgu68")

}
\keyword{ manip }
