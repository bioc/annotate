\name{chromLocation-class}
\docType{class}
\alias{chromLocation-class}
\alias{chromLocation}
\alias{species}
\alias{datSource}
\alias{nChrom}
\alias{chromNames}
\alias{chromLocs}
\alias{chromLengths}
\alias{geneToChrom}
\title{Class chromLocation, a class for describing genes and their
  chromosome mappings.}
\description{  This class provides a mechanism for mapping a set of
  genes to their respective locations in chromosomes.
  Details on how to construct an object of this class from Bioconductor
  data packags are given in the \code{byChroms} vignette in the
  \code{geneplotter} package.

}
\section{Creating Objects}{
\code{  new('chromLocation',}\cr
\code{    species      = ...., # Object of class character}\cr
\code{    datSource    = ...., # Object of class character}\cr
\code{    nChrom       = ...., # Object of class numeric}\cr
\code{    chromNames   = ...., # Object of class vector}\cr
\code{    chromLocs    = ...., # Object of class list}\cr
\code{    chromLengths = ...., # Object of class vector}\cr
\code{    geneToChrom  = ...., # Object of class genEnv}\cr
\code{  )}}
\section{Slots}{
  \describe{
    \item{\code{species}:}{Object of class "character".  The species
      that these genes correspond to. }
    \item{\code{datSource}:}{Object of class "character".  The source of
    the gene data. }
    \item{\code{nChrom}:}{Object of class "numeric". The number of
      chromosomes for this species. }
    \item{\code{chromNames}:}{Object of class "vector". A listing of
      this species' chromosome names. }
    \item{\code{chromLocs}:}{Object of class "list".  A list which
      provides specific location information for every gene. }
    \item{\code{chromLengths}:}{Object of class "vector".  A listing of
      the lengths of the chromosomes in this species. }
    \item{\code{geneToChrom}:}{Object of class "genEnv".  A hash table
      which will translate a gene identifier to its location. }
  }
}

\section{Methods}{
  \describe{
    \item{chromLengths}{(chromLocation): Gets the 'chromLengths' attribute. }
    \item{chromLocs}{(chromLocation): Gets the 'chromLocs' attribute. }
    \item{chromNames}{(chromLocation): Gets the 'chromNames' attribute. }
    \item{datSource}{(chromLocation): Gets the 'datSource' attribute. }
    \item{geneToChrom}{(chromLocation): Gets the 'geneToChrom' attribute. }
    \item{nChrom}{(chromLocation): gets the 'nChrom' attribute. }
    \item{species}{(chromLocation): gets the 'species' attribute. }
  }
}

\keyword{methods}
