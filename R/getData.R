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
        mapping <- get(paste(data, what, sep = ""))[[x]]
    }else{
        mapping <- mget(x, env=get(paste(data, what, sep="")),
                        ifnotfound=NA)
    }
    return(mapping)
}

getGO <- function(x, data) {
    lookUp(x, data, "GO")
 }

 getGOdesc <- function(x, which = c("BP", "CC", "MF", "ANY")) {
     require("GO") || stop("need the GO library")
     which <- match.arg(which)
     options(show.error.messages = FALSE)
     ans <- try(lookUp(x, "GO", "TERM"))
     options(show.error.messages = TRUE)
     if(inherits(ans, "try-error")){
         warning(paste("Invalid GO term", x))
         return(NULL)
     }else{
         if(which == "ANY"){
             return(ans)
         }else{
             ans <- ans[names(ans) == which]
             if(length(ans) == 0){
                 return(NULL)
             }else{
                 return(ans)
             }
         }
     }
 }

  getSYMBOL <- function(x, data) {
      unlist(lookUp(x, data, "SYMBOL"))
 }

  getPMID <- function(x, data) {
      lookUp(x, data, "PMID")
  }

  getLL <- function(x, data) {
      unlist(lookUp(x, data, "LOCUSID"))
  }

  if( !isGeneric("contents") && !exists("contents", mode="function") )
       setGeneric("contents", function(object)
                  standardGeneric("contents"))

  setMethod("contents", "environment",
     function(object)
         mget(ls(env=object), env=object, ifnotfound=NA))

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
