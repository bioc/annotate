\name{findNeighbors}
\alias{findNeighbors}
\alias{checkArgs}
\alias{findChr4LL}
\alias{getValidChr}
\alias{getBoundary} 
\title{A function to locate neighboring genes within a defined range
  around a target gene represented by a LocusLink id }
\description{
  Give a data package with mappings between LocsuLink ids and their
  locations on chromosomes, this function locates genes that are
  within a defined range on a given chromosome. If a LocusLink id is
  passed as one of the arguments, genes located will be neighbors to the
  gene represented by the LocusLink id within a defined range on the
  chromosome the target gene resides
}
\usage{
findNeighbors(chrLoc, llID, chromosome, upBase, downBase)
checkArgs(llID, chromosome, upBase, downBase)
findChr4LL(llID, chrEnv, organism)
getValidChr(organism)
getBoundary(loc, base, lower = TRUE)
} 
\arguments{
  \item{chrLoc}{\code{chrLoc} a character string for the name of the
    data package that contains mappings between LocusLink ids and their
    locations on chromosomes. For each chromosome, there assumed to be
    mappings for the strat and end locations of genes represented by
    LocusLink ids}
  \item{llID}{\code{llID} a character string for the LocusLink id
    representing a gene whose nighbors are sought. llID can be missing}
  \item{chromosome}{\code{chromosome} a character string for the number
    of the chromosome of interest. chromosome is only required for
    locating genes within a range on the chromosome}
  \item{upBase}{\code{upBase} a numeric or character string for the
    number of base pairs that defines the upper limit of the range to
    locate genes. If neighbors of a given gene is sourght, the value
    will be the distance in number of base pairs from the target gene
    upstream, to which search for genes will be conducted. Otherwise, the
    value will be the upper limit in number of base pairs from the p arm,
    to which search for genes will be conducted}
  \item{downBase}{\code{downBase} a numeric or character string for the
    number of base pairs that defines the lower limit of the range to
    locat gene. If neighbors of a given gene is sourght, the value
    will be the distance in number of base pairs from the target gene
    downstream, to which search for genes will be conducted. Otherwise, the
    value will be the lower limit in number of base pairs from the p arm,
    to which search for genes will be conducted}
  \item{organism}{\code{organism} a character string for the name of the
    organism of interest}
  \item{chrEnv}{\code{chrEnv} an environment object with keys for
    LocusLink ids and values for the chromosomes where genes reside}
  \item{loc}{\code{loc} a numeric of character string for the
    chromosomal location of gene of interest}
  \item{base}{\code{base} either a \code{downBase} or \code{upBase}}
  \item{lower}{\code{lower} a boolean indicating whether the lower or
    upper boundary of search limit is sought}
}
\details{
  A chrLoc data package can be created using function
  chrLocPkgBuilder of AnnBuilder, in which locusLink ids
  are mapped to location data on individual chromosomes.

  Genes are considered to be neighbors to a given target gene or within
  a given range when the transcription of genes start and end within the
  given range.

  findNeighbors, checkArgs, findChr4LL, getValidChr, and getBoundary are
  accessory functions called by findNeighbors and may not have real
  values outside.  
}
\value{
  The function returns a list of named vectors. The length of the list
  is one when genes in a given rwgion are sought but varies depending on
  whether a given gene can be mapped to one or more chromosomes when
  neighoring genes of a target gene are sought. Names of vector can be
  "Confident" when a gene can be confidently placed on a chromosome or
  "Unconfident" when a gene can be placed on a chromosome but its exact
  location can not be determined with great confidence.
}
\references{\url{http://www.genome.ucsc.edu/goldenPath/}}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide bioinformatics functionalities through R}

\examples{
if(require(humanCHRLOC)){
   findNeighbors("humanCHRLOC", "10228", upBase = 600000, downBase = 600000)
}else{
   print("Can not find neighbors without the required data package")
}
}
\keyword{manip}
