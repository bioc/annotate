\name{GOTerms-class}
\docType{class}
\alias{GOTerms}
\alias{GOId}
\alias{Category}
\alias{Definition}
\alias{Ontology}
\alias{Secondary}
\alias{Synonym}
\alias{GOTerms-class}
\alias{Category,GOTerms-method}
\alias{Definition,GOTerms-method}
\alias{GOId,GOTerms-method}
\alias{Ontology,GOTerms-method}
\alias{Secondary,GOTerms-method}
\alias{Synonym,GOTerms-method}
\alias{print,GOTerms-method}

\title{Class "GOTerms"}
\description{A class to represent Gene Ontology terms for a given node}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("GOTerms",
    GOId, Term, Category, Synonym, Secondary, Definition)}.
  GOId, Term, and Category are required.
}
\section{Slots}{
  \describe{
    \item{\code{GOId}:}{Object of class \code{"character"} A character
      stirng for the GO id of a primary node}
    \item{\code{Ontology}:}{Object of class \code{"character"} A
      character string that defines the role of gene product
      corresponding to the primary GO id}
    \item{\code{Synonym}:}{Object of class \code{"character"} other
      synonymous ontology terms of the primary GO id}
    \item{\code{Secondary}:}{Object of class \code{"character"} GO ids
      that secondary to the primary GO id as results of merging}
    \item{\code{Definition}:}{Object of class \code{"character"} Further
      definition of the ontology of the primary GO id}
    \item{\code{Category}:}{Object of class \code{"character"} Gene
      Ontology category. Can be MF - molecular function, CC - cellular
      component, or BP - biological process}
  }
}
\section{Methods}{
  \describe{
    \item{Category}{\code{signature(object = "GOTerms")}: The get method
      for slot Category}
    \item{Definition}{\code{signature(object = "GOTerms")}: The get
      method for slot Definition}
    \item{GOId}{\code{signature(object = "GOTerms")}: The get method for
      slot GOId}
    \item{Ontology}{\code{signature(object = "GOTerms")}: The get method
      for slot Ontology}
    \item{Secondary}{\code{signature(object = "GOTerms")}: The get
      method for slot Secondary}
    \item{Synonym}{\code{signature(object = "GOTerms")}: The get method
      for slot Synonym}
    \item{print}{\code{signature(x = "GOTerms")}: The method for pretty
      print}
  }
}
\references{\url{http://www.geneontology.org/}}
\author{Jianhua Zhang}
\note{GOTerms objects are used to represent GO terms for primary GO
  nodes in the annotation data package GO}

\seealso{
  \code{\link{GOPkgBuilder}}
}
\examples{
  goTerms <- new("GOTerms", GOId = "GO:1234567", Ontology = "Test",
                 Category = "MF")
  GOId(goTerms)
  Ontology(goTerms)
  Category(goTerms)
}
\keyword{classes}










