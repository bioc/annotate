\name{compatibleVersions}
\alias{compatibleVersions}

\title{function to check to see if the packages represented by the names
passed have the same version number}
\description{
  This function takes the names of installed R packages and then checks
  to see if they all have the same version number.
}
\usage{
compatibleVersions(...)
}

\arguments{
  \item{\dots}{\code{\dots} character strings for the names of R
    packages that have been installed}
}
\details{
  If all the package have the same version number, the function returns
  TRUE. Otherwise, the function returns FALSE
}
\value{
  This function returns TRUE or FALSE depending on whether the packages
  have the same version number
}

\author{Jianhua Zhang}

\seealso{\code{\link{packageDescription}}}
\examples{
if(require("hgu95av2") && require("GO")){
    compatibleVersions("hgu95av2", "GO")
}
}
\keyword{misc}
