\name{LL2homology}
\alias{LL2homology}
\alias{HGID2homology}
\alias{ACC2homology}
\title{Functions that find the homology data for a given set of
  LocusLink ids or HomoloGeneIDs}
\description{
  Given a set of LocusLink ids or NCBI HomoloGeneIDs, the functions obtain the
  homology data and represent them as a list of sub-lists of homoData
  objects using the homology data package. A sub-list can be of
  length 1 or greater depending on whether a LocusLink id can be mapped
  to one or more HomoloGeneIDs.
}
\usage{
LL2homology(llids)
HGID2homology(hgid)
ACC2homology(accs)
}
\arguments{
  \item{llids}{\code{llids} a vector of character strings or numberic
    numbers for a set of LocusLink ids whose homologous genes in other
    organisms are to be found}
  \item{hgid}{\code{hgid} a named vector of character strings or numberic
    numbers for a set of HomoloGeneIDs whose homologous genes in other
    organisms are to be found. Names of the vector give the code used by
    NCBI for organisms}
  \item{accs}{\code{accs} a vector of character strings for a set of
    GenBank Accession numbers}
}
\details{
  The homology data package has to be installed before executing any of
  the two functions.

  A homoData object has the following slots:

  homoOrg - a named vector of a single character string whose value
  is the scientific name of the organism and name the numeric code
  used by NCBI for the organism.
  
  homoLL - an integer for LocusLink id.
  
  homoHGID - an integer for internal HomoloGeneID.
  
  homoACC - a character string for GenBank accession number of the
  best matching sequence of the organism.
  
  homoType - a single letter for the type of similarity measurement
  between the homologous genes. homoType can be either B (reciprocal
  best best between three or more organisms), b (reciprocal best
  match between two organisms), or c (curated homology relationship
  between two organisms).
  
  homoPS - a percentage value measured as the percent of identity of
  base pair alignment between the homologous sequences. 
  
  homoURL - a url to the source if the homology relationship is a
  curated orthology.
  
  HomoData objects with homoType = B or b will not have any value
  for homoURL and objects with homoType = c will not have any value
  for homoPS.
  
  The value of a given slot of a homoData object can be accessed by
  using homoOrg(xxx) for the homoOrg slot of the homoData object
  xxx, for example.
}
\value{
  Both functions returns a list of homoData objects containing data for
  homologous genes in other organisms.
}
\references{\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?=homologene}}
\author{Jianhua Zhang}

\seealso{\code{\link{homoData-class}}}
\examples{
  if(require("homology")){
      llids <- ls(env = homologyLL2HGID)[2:5]
      LL2homology(llids)
  }
}
\keyword{misc}

