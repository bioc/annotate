\name{GOTerms-class}
\docType{class}
\alias{GOTerms}
\alias{GOID}
\alias{Ontology}
\alias{Definition}
\alias{Term}
\alias{Secondary}
\alias{Synonym}
%\alias{show}
\alias{GOTerms-class}
\alias{Ontology,GOTerms-method}
\alias{Ontology,ANY-method}
\alias{Definition,GOTerms-method}
\alias{GOID,GOTerms-method}
\alias{Term,GOTerms-method}
\alias{Secondary,GOTerms-method}
\alias{Synonym,GOTerms-method}
\alias{show,GOTerms-method}

\title{Class "GOTerms"}
\description{A class to represent Gene Ontology terms for a given node}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("GOTerms",
    GOId, Term, Category, Synonym, Secondary, Definition)}.
  GOId, Term, and Category are required.
}
\section{Slots}{
  \describe{
    \item{\code{GOID}:}{Object of class \code{"character"} A character
      stirng for the GO id of a primary node}
    \item{\code{Term}:}{Object of class \code{"character"} A
      character string that defines the role of gene product
      corresponding to the primary GO id}
    \item{\code{Synonym}:}{Object of class \code{"character"} other
      ontology terms that are considered to be synonymous to the primary
      term attached to the GO id (e.g. type I programmed cell death is a
      synonym of apoptosis). Synonymous here can mean that the
      synonym is an exact synonym of the primary term, is related to the
      primary term, is broader than the primary term, is more precise
      than the primary term, or  name is related to the term, but is not
      exact, broader or narrower.}
    \item{\code{Secondary}:}{Object of class \code{"character"} GO ids
      that are secondary to the primary GO id as results of merging GO
      terms so that One GO id becomes the primary GO id and the rest
      become the secondary}
    \item{\code{Definition}:}{Object of class \code{"character"} Further
      definition of the ontology of the primary GO id}
    \item{\code{Ontology}:}{Object of class \code{"character"} Gene
      Ontology category. Can be MF - molecular function, CC - cellular
      component, or BP - biological process}
  }
}
\section{Methods}{
  \describe{
    \item{Ontology}{\code{signature(object = "GOTerms")}: The get method
      for slot Ontology}
    \item{Ontology}{\code{signature(object = "ANY")}: The get method
      for slot Ontology handles missing values.}
    \item{Definition}{\code{signature(object = "GOTerms")}: The get
      method for slot Definition}
    \item{GOID}{\code{signature(object = "GOTerms")}: The get method for
      slot GOID}
    \item{Term}{\code{signature(object = "GOTerms")}: The get method
      for slot Term}
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

\examples{
  goTerms <- new("GOTerms", GOID = "GO:1234567", Term = "Test",
                 Ontology = "MF")
  GOID(goTerms)
  Term(goTerms)
  Ontology(goTerms)
}
\keyword{classes}

