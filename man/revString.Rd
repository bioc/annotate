\name{revString}
\alias{revString}
\alias{revBase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions to create anti-sense DNA string }
\description{
  These functions will reverse a DNA character string and then convert
  to its complement. Useful for matching to DNA sequences when you have
  the wrong strand sequence.
}
\usage{
revString(x)
revBase(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A character string or vector of character strings
    representing a DNA sequence. By default this should only
    contain A, C, G, T, and possibly N.  }
}

\value{
  A character string or vector of character strings.
}
\author{James W. MacDonald }
\examples{
revString("AAACGCTGCGTTTG")
}
\keyword{misc }