\name{usedChromGenes}
\alias{usedChromGenes}
\title{A function to select used genes on a chromosome from an exprSet.}
\description{
  Given an instance of an exprSet and a chromLocation object, as well
  as the name of a chromosome - this function will select out of the
  chromLocation object all the genes represented in the exprSet from
  the requested chromosome.
}
\usage{
usedChromGenes(eSet, chrom, specChrom)
}
\arguments{
  \item{eSet}{An instance of an exprSet object.}
  \item{chrom}{The name of the chromosme of interest.}
  \item{specChrom}{An instance of a chromLocation object.}
}
\value{
  Returns a vector of gene names that represent the genes from the
  requested chromosome that exist in the exprSet.
}
\author{ Jeff Gentry}
\examples{
    data(eset)
    data(hgu95ProbLocs)
    usedChromGenes(eset,"1",hgu95ProbLocs)
}
\keyword{ interface }

  