\name{getTDRows}
\alias{getTDRows}
\alias{getQueryLink}
\alias{getQuery4UG}
\alias{getQuery4LL}

\title{Functions to create hypertext links that can be placed in a table
  cell of a HTML file} 
\description{
  Given a vector of ids, the functions will create a vector of hypertext
  links to a defined public repositories such as LocusLink, UniGene
  .... The linkages can be placed in a html file constructed by
  \code{\link{ll.htmlpage}} 
}
\usage{
getTDRows(ids, repository = "ug")
getQueryLink(ids, repository = "ug")
getQuery4UG(ids)
getQuery4LL(ids)
}

\arguments{
  \item{ids}{\code{ids} a vector of character string for ids that will
    used to construct hypertext links}
  \item{repository}{\code{repository} A character string for the name of
    a public repositories. Valid values include "ll", "ug"} 
}
\details{
  \code{\link{getQuery4LL}} constructs hypertext links to LocusLink
  using the provided ids. 

  \code{\link{getQuery4UG}} constructs hypertext links to UniGene using
  the provided ids.

  \code{\link{getQueryLink}} directs calls to construct hypertext links
  using the provided ids.

  \code{getTDRows}} directs calls to construct hypertext links
  using the provided ids and then places the links in table cells.
}
\value{
  Returns a vector of character strings representing the hypertext links.
}

\author{Jianhua Zhang}
\note{The functions are part of Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{ll.htmlpage}}}
\examples{
ids <- c("Hs.78713", "Hs.11184", "Hs.281866","Hs.31314")
getTDRows(ids)  
}
\keyword{manip}
