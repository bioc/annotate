\name{blastSequences}
\alias{blastSequences}

\title{
  Run a blast query to NCBI for either a string or an entrez gene ID and
  then return a series of MultipleAlignment objects.
}

\description{
  This function sends a query to NCBI as a string of sequence or an
  entrez gene ID and then returns a series of MultipleAlignment objects.
}

\usage{
  blastSequences(x, database, hitListSize, filter, expect, program,
      timeout=40, parse.result=TRUE)
}

\arguments{
  \item{x}{
    A sequence as a character vector or an integer corresponding to an
    entrez gene ID.
  }
  \item{database}{
    Which NCBI database to use. If not \dQuote{blastn}, then set
    \code{parse.result=FALSE}
  }
  \item{hitListSize}{
    Number of hits to keep.
  }
  \item{filter}{
    Sequence filter; \dQuote{L} for Low Complexity, \dQuote{R} for Human Repeats,
    \dQuote{m} for Mask lookup
  }
  \item{expect}{
    The BLAST \sQuote{expect} value above which matches will be
    returned.
  }
  \item{program}{
    Which program do you want to use for blast.
  }
  \item{timeout}{
    Approximate maximum length of time, in seconds, to wait for a result.
  }
  \item{parse.result}{
    logical(1) indicating whether the result should be parsed from the
    XML format returned by the server to a list of
    \code{\linkS4class{DNAMultipleAlignment}} instances.
  }

}

\details{
  Right now the function only works for "blastn".

  The NCBI URL api used by this function is documented at
  \url{http://www.ncbi.nlm.nih.gov/blast/Doc/urlapi.html}
}

\value{

  A series of MultipleAlignment objects of the appropriate type or, if
  \code{parse.result=FALSE}, an XML document returned from the NCBI
  server. The XML document is described by
  \url{http://www.ncbi.nlm.nih.gov/dtd/NCBI_BlastOutput.dtd} and
  \url{http://www.ncbi.nlm.nih.gov/dtd/NCBI_BlastOutput.mod.dtd}. }

\author{
  M. Carlson
}

\examples{

## x can be an entrez gene ID
blastSequences(17702, timeout=40)

if (interactive()) {

    ## or x can be a sequence
    blastSequences(x = "GGCCTTCATTTACCCAAAATG")

    ## hitListSize does not promise that you will get the number of
    ## matches you want..  It will just try to get that many.
    blastSequences(x = "GGCCTTCATTTACCCAAAATG", hitListSize="20")

}
}
