\name{serializeEnv}
\alias{serializeEnv}
\title{A Function To Serialize Environment}
\description{
  This function is primarily a wrapper around the \code{.saveRDS}
  function.  It takes the name of an environment and will serialize the
  environment to a file, either as the environment or as a list
  representation.  
}
\usage{
serializeEnv(env, fname, asList=TRUE, ...)
}
\arguments{
  \item{env}{The name of the environment to serialize.}
  \item{fname}{The name of the output file.}
  \item{asList}{Whether to convert the environment to a list first.}
  \item{\dots}{Other arguments to pass on to \code{\link{.saveRDS}}}
}
\details{
  The output file will default to using a binary format, as per the
  defaults of \code{\link{.saveRDS}}, if users wish to use an ASCII
  file, passing in \code{ascii=TRUE} to the \code{\dots} argument will
  suffice.

  The environment stored can be retrieved using the
  \code{\link{.readRDS}} function.
  
  The default for \code{asList} is to provide the environment as a list
  in the serialized output file.  Care should be taken by the user to
  know which was the case when using \code{\link{.readRDS}}.
}
\author{Jeff Gentry}
\seealso{\code{\link{.saveRDS}}, \code{\link{.readRDS}}}
\examples{
   z <- new.env()
   assign(z, "a", 1)
   assign(z, "b", 2)
   assign(z, "c", 3)
   serializeEnv(z, tempfile())
}
\keyword{utilities}
