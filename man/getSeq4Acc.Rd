\name{getSeq4Acc}
\alias{getSeq4Acc}
\alias{getGI}
\alias{getSEQ}

\title{Queries the NCBI database to obtain the sequence for a given
  GenBank Accession number}
\description{
  Given a GenBank Accession number, getSeq4Acc queries the NCBI database for
  the nucleotide sequence.  
}
\usage{
getSeq4Acc(accNum)
getGI(accNum)
getSEQ(gi)
}

\arguments{
  \item{accNum}{\code{accNum} a character string for a GenBank Accession
    number (i.e. M22490)}
  \item{gi}{\code{gi} a character string or numberic numbers for the gi
    number corresponding to \code{accNum}}
}
\details{
  The NCBI database is first quired for a gi number associated with the
  given GenBank Accession number. The gi number is then used to obtain
  the FASTA file for the nucleotide sequence.

  getGI and getSEQ are help functions of getSeq4Acc.
}
\value{
  getSeq4Acc returns a character string of nucleotide sequence
}
\references{\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi}}
\author{Jianhua Zhang}

\examples{
getSeq4Acc("M22490")
}
\keyword{misc}

