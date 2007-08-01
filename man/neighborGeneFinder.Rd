\name{neighborGeneFinder}
\alias{neighborGeneFinder}
\title{A widget for locating genes neighboring a target gene}
\description{
  Given a set of data (matrix) with entries for LocusLink or UniGene
  ids, the neighboring genes of a gene selected from a list on the
  interface can be located.
}
\usage{
neighborGeneFinder(geneData, keyName = c("unigene", "locuslink"),
organism = c("human", "mouse", "rat")) 
}
\arguments{
  \item{geneData}{\code{geneData} a matrix with columns named. The name
    for one of the columns has to be either "locuslink" or "unigene"}
  \item{keyName}{\code{keyName} a character string for the name of the
    key columns of \code{geneData}. Has to be either "locuslink" or
    "unigene"} 
  \item{organism}{\code{organism} a character string for the name of the
    organism of interest. Has to be "human", "mouse", or "rat"} 
}
\details{
  Bioconductor's data package XXXCHRLLOC has to be instatlled for the
  widget to work. If \code{keyName} is "unigene", XXXLLMappings is
  required, where XXX is the name of the organism of interest.
}
\value{
  This function returns a list of lists. Elements of the top level list
  are either LocusLink or UniGene ids. A sublist is inturn a list of
  lists whose top level elements are chromosome numbers, each of which
  is a list with an "upstream" and "downstream" elements.
}
\author{Jianhua Zhang}

\examples{
  if(interactive()){
     require("annotate", character.only = TRUE) ||
     stop("Package annotate is not availble")
     geneData <- cbind(paste("100", 1:16, "_at", sep = ""), c(1, 50,
                       10044, 51, 71, 51371, 81, 51426, 188, 293, 360,
                       364, 375, 387, 513, 10572))
     colnames(geneData) <- c("Probe", "locuslink")
     neighborGeneFinder(geneData, "locuslink", "human")
  }
  
}
\keyword{interface}

