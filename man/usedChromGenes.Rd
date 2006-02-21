\name{usedChromGenes}
\alias{usedChromGenes}
\title{A function to select used genes on a chromosome from an exprSet.}
\description{
  Given an instance of an \code{exprSet}, a \code{chromLocation} object
  and the name of a chromosome this function returns all genes represented
  in the \code{exprSet} on the specified chromosome.
}
\usage{
usedChromGenes(eSet, chrom, specChrom)
}
\arguments{
  \item{eSet}{An instance of an \code{exprSet} object.}
  \item{chrom}{The name of the chromosome of interest.}
  \item{specChrom}{An instance of a \code{chromLocation} object.}
}
\value{
  Returns a vector of gene names that represent the genes from the
  \code{exprSet} that are on the specified chromosome.
}
\author{ Jeff Gentry}
\examples{
    data(sample.exprSet.1)
    data(hgu95AProbLocs)
    usedChromGenes(sample.exprSet.1,"1",hgu95AProbLocs)
}
\keyword{interface}
