\name{findNeighbors}
\alias{findNeighbors}
\title{A function to locate neighboring genes within a defined range
  around a target gene represented by a LocusLink id }
\description{
  Give a data package with mappings between LocsuLink ids and their
  locations on chromosomes and a LocusLink id, this function locates
  genes that are neighbors within a defined range of the target gene on
  a given chromosome.
}
\usage{
findNeighbors(chrLoc, llID, chromosome, upBase, downBase, agreessive = TRUE)
} 
\arguments{
  \item{chrLoc}{\code{chrLoc} a character string for the name of the
    data package that contains mappings between LocusLink ids and their
    locations on chromosomes. For each chromosome, there assumed to be
    mappings for the strat and end locations of genes represented by
    LocusLink ids}
  \item{llID}{\code{llID} a character string for the LocusLink id
    representing a gene of interest}
  \item{chromosome}{\code{chromosome} a character string for the number
    of the chromosome of interest}
  \item{upBase}{\code{upBase} a numeric or character string for the
    number of base pairs that defines the upper limit of the range to
    search for neighbors from the location of a target gene}
  \item{downBase}{\code{downBase} a numeric or character string for the
    number of base pairs that defines the lower limit of the range to
    search for neighbors from the location of a target gene}
  \item{agreessive}{\code{agreessive} a boolean indicating whether the
    search will be conducted in an agreessinve way}
}
\details{
  A chrLoc data package can be created using function
  chrLocPkgBuilder of AnnBuilder, in which locusLink ids
  are mapped to location data on individual chromosomes.

  An agreessive search for genes whose transcription start within the
  upper limit or ends within the lower limit. Otherwise, search will be
  conducted for genes whose transcription ends within the upper limit or
  starts within the lower limit.
}
\value{
  The function returns a vector of character strings of LocusLink ids or
  NULL if no neighbor has been found within the given reange around the
  target gene
}
\references{\url{http://www.genome.ucsc.edu/goldenPath/}}
\author{Jianhua Zhang}
\note{This function is part of the bioconductor project at Dana-Farber
  Cancer Institute to provide bioinformatics functionalities through R}

\examples{
if(require(humanCHRLOC)){
   findNeighbors("humanCHRLOC", "10228", "1", 1350500, 1350000)   
}else{
   print("Can not find neighbors without the required data package")
}
}
\keyword{manip}

