\name{getQueryLink}
\alias{getQueryLink}
\alias{getQuery4UG}
\alias{getQuery4SP}
\alias{getQuery4OMIM}
\alias{getQuery4LL}
\alias{getQuery4GB}
\alias{getQuery4Affy}
\alias{getQuery4FB}
\alias{getQuery4EN}
\alias{getCells}
\alias{getTDRows}

\title{Functions to create hypertext links that can be placed in a table
  cell of a HTML file }
\description{
  Given a vector of ids, the functions will create a vector of
  hypertext links to a defined public repositories such as
  LocusLink, UniGene .... The linkages can be placed in a html file
  constructed by \code{\link{htmlpage}}
}
\usage{
getQueryLink(ids, repository = "ug")
getTDRows(ids, repository = "ug")
getCells(ids, repository = "ug")
getQuery4LL(ids)
getQuery4UG(ids)
getQuery4SP(ids)
getQuery4GB(ids)
getQuery4OMIM(ids)
getQuery4Affy(ids)
getQuery4FB{ids}
getQuery4EN(ids)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ids}{ A character vector of ids, or alternatively, a list
    containing character vectors of ids. These will be used to construct
    hypertext links. A list should be used in cases where there are
    multiple ids per gene.}
  \item{repository}{ A character string for the name of a public
    repository. Valid values include "ll", "ug", "gb", "sp", "omim",
    "affy", "en", and "fb". }
}
\details{
 \code{\link{getQuery4LL}} constructs hypertext links to LocusLink using the
 provided ids.
 
 \code{\link{getQuery4GB}} constructs hypertext links to GenBank using the
 provided ids.
 
 \code{\link{getQuery4UG}} constructs hypertext links to UniGene using the
 provided ids.
 
 \code{\link{getQuery4Affy}} constructs hypertext links to Affymetrix using the
 provided ids.
 
 \code{\link{getQuery4SP}} constructs hypertext links to SwissProt using the
 provided ids.
 
 \code{\link{getQuery4OMIM}} constructs hypertext links to OMIM using the
 provided ids.

 \code{\link{getQuery4FB}} constructs hypertext links to FlyBase using
 the provided ids.

 \code{\link{getQuery4EN}} constructs hypertext links to EntrezGene
 using the provided ids. This supercedes \code{getQuery4LL}, as
 LocusLink has been deprecated by NCBI.
 
 \code{\link{getQueryLink}} directs calls to construct hypertext links using
 the provided ids.
 
 \code{\link{getTDRows}} constructs each row of the resulting table.
 
 \code{\link{getCells}} constructs each cell of the resulting table.

 Note that some of these functions (\code{getQuery4OMIM},
 \code{getQuery4UG}, \code{getQuery4LL}), \code{getQuery4FB} attempt to
 return empty cells for ids that don't make sense, rather than broken
 links. For the other getQuery4XX functions, the end user must replace
 all nonsense ids with "&nbsp;" in order to have an empty cell. 
 
}
\value{
 Returns a vector of character strings representing the hypertext links.
}

\author{ Jianhua Zhang <jzhang@jimmy.harvard.edu> with further
  modifications by James W. MacDonald <jmacdon@med.umich.edu> }
\note{  The functions are part of Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through
  R
}


\keyword{ manip }% __ONLY ONE__ keyword per line
