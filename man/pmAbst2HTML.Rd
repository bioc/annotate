\name{pmAbst2HTML}
\alias{pmAbst2HTML}
\alias{pmAbst2html2}
\title{HTML Generation for PubMed Abstracts}
\description{
  This function will take a \code{pubMedAbst} object, or a list of these
  objects and generate a simple web page that will list the titles of
  the abstracts and link to their full page on PubMed
}
\usage{
pmAbst2HTML(absts, filename, title, simple = TRUE, table.center = TRUE)
}
\arguments{
  \item{absts}{A list of \code{pubMedAbst} (or a single object)}
  \item{filename}{The output filename.  Defaults to absts.html}
  \item{title}{ Extra title information for your listing}
  \item{simple}{ Whether just to list the title & publication date of
    the paper or full information.  Currently unimplmented.}
  \item{table.center}{If TRUE, will center the listing of abstracts}
}
\details{
  This function uses the \code{Entrez} functionality provided by NCBI to
  retrieve the abstract URL at the PubMed site.  It will then create a
  tabular webpage which will list the titles of the abstracts provided
  and have them link to the appropriate PubMed page.
}
\value{
  A file with the name provided by \code{filename} containing a simple
  HTML file.
}
\author{Jeff Gentry}
\note{ The functions are part of Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{pubMedAbst}}
\examples{
        x <- pubmed("9695952","8325638","8422497")
        a <- xmlRoot(x)
        numAbst <- length(xmlChildren(a))
        absts <- list()
        for (i in 1:numAbst) {
           absts[[i]] <- buildPubMedAbst(a[[i]])
        }
        fname <- tempfile()
        pmAbst2HTML(absts,filename=fname)
        file.remove(fname)
}
\keyword{utilities}
