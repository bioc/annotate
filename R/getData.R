##copyright 2002 R. Gentleman, all rights reserved
##helper functions for dealing with data environments (soon to be hash
##tables)

## JZ added lookUp and modified the other functions so that they all
## use lookUp. Nov. 6, 2003.
lookUp <- function(x, data, what){
    if(length(x) < 1){
        stop("No key(s) provided")
    }
    require(data, quietly = TRUE, character.only = TRUE) ||
        stop(paste("Data package", data, "not available"))
    if(!any(ls(match(paste("package:", data, sep = ""),
                     search())) == paste(data, what, sep = ""))){
        stop(paste(what, "is an invalid element name"))
    }
    if(length(x) == 1){
        mapping <- get(x, env = get(paste(data, what, sep = "")))
    }else{
        mapping <- multiget(x, env = get(paste(data, what, sep = "")))
    }
    return(mapping)
}

getGO <- function(x, data) {
     # library(data, character.only=TRUE)
     # GOenv <- get(paste(data, "GO",sep=""))
     #multiget(x, env=GOenv)
    lookUp(x, data, "GO")
 }

 getGOdesc <- function(x, which=c("MF","BP","CC") ) {
     #require("GO") || stop("need the GO library")
     #d <- match.arg(which)
     #de <- switch(d, MF=GOMFID2TERM, BP=GOBPID2TERM, CC=GOCCID2TERM,
     #             stop(paste(which, "did not match a GO data type")))
     #ans <- multiget(x, env=de)
     #ans[is.na(ans)] <- NULL
     #ans
     ans <- lookUp(x, "GO", paste(which, "ID2TERM", sep = ""))
     ans[is.na(ans)] <- NULL
     ans
 }

  getSYMBOL <- function(x, data) {
     #library(data, character.only=TRUE)
     #GOenv <- get(paste(data, "SYMBOL",sep=""))
     #unlist(multiget(x, env=GOenv))
      unlist(lookUp(x, data, "SYMBOL"))
 }

  getPMID <- function(x, data) {
      #library(data, character.only=TRUE)
      #PMenv <- get(paste(data, "PMID", sep=""))
      #multiget(x, env=PMenv)
      lookUp(x, data, "PMID")
  }

  getLL <- function(x, data) {
      #library(data, character.only=TRUE)
      #LLenv <- get(paste(data, "LOCUSID", sep=""))
      #unlist(multiget(x, env=LLenv))
      unlist(lookUp(x, data, "LOCUSID"))
  }

  if( !isGeneric("contents") && !exists("contents", mode="function") )
       setGeneric("contents", function(object)
                  standardGeneric("contents"))

  setMethod("contents", "environment",
     function(object)
         multiget(ls(env=object), env=object))

installDataPackage <- function(pkga, liba=.libPaths()[1]) {
    require("reposTools")||
              stop("installDataPackage requires package reposTools")

    z <- getReposEntry("BIOCData")
    x <- install.packages2(pkga, z, lib=liba)
    if (length(statusList(x)) == 0)
        stop(paste("Data package",pkga,"does not seem to exist",
                   "in the Bioconductor\ndata package repository."))
}

# This function needs to be updated when new annotation items are
# added to the data packages
getUniqAnnItem <- function(){
    return(c("ACCNUM", "LOCUSID", "GENENAME", "SYMBOL", "MAP",
             "GRIF", "SUMFUNC", "NM", "NP"))
}
