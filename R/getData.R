##copyright 2002 R. Gentleman, all rights reserved
##helper functions for dealing with data environments (soon to be hash
##tables)


### Define the contents method
if( !isGeneric("contents") && !exists("contents", mode="function") )
    setGeneric("contents", function(object)
               standardGeneric("contents"))

setMethod("contents", "environment",
          function(object)
          multiget(ls(env=object), env=object))

## Done w/ contents method

getGO <- function(x, data="hgu95a") {
     library(data, character.only=TRUE)
     GOenv <- get(paste(data, "GO",sep=""))
     multiget(x, env=GOenv)
 }

 getGOdesc <- function(x, which=c("MF","BP","CC") ) {
     require(GO) || stop("need the GO library")
     d <- match.arg(which)
     de <- switch(d, MF=GOMFID2TERM, BP=GOBPID2TERM, CC=GOCCID2TERM,
                  stop(paste(which, "did not match a GO data type")))
     ans <- multiget(x, env=de)
     ans[is.na(ans)] <- NULL
     ans
 }

  getSYMBOL <- function(x, data="hgu95a") {
     library(data, character.only=TRUE)
     GOenv <- get(paste(data, "SYMBOL",sep=""))
     unlist(multiget(x, env=GOenv))
 }

  getPMID <- function(x, data="hgu95a") {
      library(data, character.only=TRUE)
      PMenv <- get(paste(data, "PMID", sep=""))
      multiget(x, env=PMenv)
  }

  getLL <- function(x, data="hgu95a") {
      library(data, character.only=TRUE)
      LLenv <- get(paste(data, "LOCUSID", sep=""))
      unlist(multiget(x, env=LLenv))
  }


installDataPackage <- function(pkg, lib=.libPaths()[1]) {
    require(reposTools)||stop("installDataPackage requires package reposTools")

    z <- getReposEntry("http://www.bioconductor.org/data/dataRepos")
    x <- install.packages2(pkg, z, lib=lib)
    if (length(statusList(x)) == 0)
        stop(paste("Data package",pkg,"does not seem to\nexist",
                   "in the Bioconductor data package repository."))
}
