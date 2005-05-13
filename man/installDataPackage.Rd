\name{installDataPackage}
\alias{installDataPackage}
\title{A function to install data packages from the Internet}
\description{
  This function can be used to easily download and install a data
  package from the Bioconductor website.
}
\usage{
installDataPackage(pkga, liba = .libPaths()[1])
}
\arguments{
  \item{pkga}{The name of the desired data package}
  \item{liba}{The install library, defaulting to the base library}
}
\details{
  installDataPackage uses the \code{reposTools} package to perform the
  actual download and install - this function is mainly just a
  simplified wrapper for \code{install.packages2}.  It connects to the
  data package repository at \url{http://www.bioconductor.org/data/dataRepos},
  and attempts to get the requested package.
}
\author{Jeff Gentry}
\seealso{\code{\link[reposTools]{install.packages2}}}
\examples{
  z <- tempfile()
  dir.create(z)
  installDataPackage("GO", liba=z)
}
\keyword{utilities}
