##Copyright R. Gentleman, 2004
##simple functions to get Evidence codes

##get then GO term names for a particular (sub)ontology
getOntology = function(inlist, ontology=c("MF", "BP", "CC")) {
   which = match.arg(ontology)
   onts = sapply(inlist, function(z) z$Ontology)
   onts = onts[!is.na(onts)]
   unique(names(inlist[onts %in% which]))
}


##get GO evidence codes
getEvidence = function(inlist) 
     sapply(inlist, function(z) z$Evidence)

##drop a specified set of evidence codes
dropECode = function(inlist, code = "IEA") {
    hasCode = sapply(inlist, function(z) z$Evidence)
    badVals = hasCode %in% code
    inlist[!badVals]
}


## helper function, determines if there is a GO annotation for the
## desired mode
hasGOannote <- function(x, which="MF") {
    if (is(x, "GOTerms")) {
        cat <- Ontology(x)
        if (!is.na(cat) && cat == which)
          return(TRUE) else return(FALSE)
    }
    if (is.list(x)) {
        gT <- sapply(x, function(y) is(y, "GOTerms"))
        if (any(gT)) {
            if (all(gT)) {
                cats <- sapply(x, Ontology)
                return(cats == which)
            }
            else
              stop("mixed arguments not allowed")
        }
    }
    if (!is.character(x))
      stop("wrong argument")
    tm <- getGOOntology(x)
    return(tm == which)
}


##three functions to get all the GO information for a set of GO terms
##FIXME: these need to be renovated - probably removed even..
 getGOOntology <- function(x) {
     if( !is.character(x) )
         stop("need a character argument")
     if(length(x) == 0 )
         return( character(0))
     wh <- mget(x, env=GOTERM, ifnotfound=NA)
     return( sapply(wh, Ontology) )
 }

 getGOParents <- function(x) {
     if( !is.character(x) )
         stop("need a character argument")
     if(length(x) == 0 )
         return(list())
     hasMF <- mget(x, env=GOMFPARENTS, ifnotfound=NA)
     hasBP <- mget(x, env=GOBPPARENTS, ifnotfound=NA)
     hasCC <- mget(x, env=GOCCPARENTS, ifnotfound=NA)
     lenx <- length(x)
     rval <- vector("list", length=lenx)
     names(rval) <- x
     rval <- vector("list", length=lenx)
     names(rval) <- x
     for(i in 1:lenx) {
         if( (length(hasMF[[i]]) > 1 ) || !is.na(hasMF[[i]]) )
             rval[[i]] <- list(Ontology="MF", Parents=hasMF[[i]])
         else if( (length(hasMF[[i]]) > 1 ) || !is.na(hasBP[[i]]) )
             rval[[i]] <- list(Ontology="BP", Parents=hasBP[[i]])
         else if( (length(hasMF[[i]]) > 1 ) || !is.na(hasCC[[i]]) )
             rval[[i]] <- list(Ontology="CC", Parents=hasCC[[i]])
         else
             stop(paste(x[i], "is not a member of any ontology"))
     }
     return(rval)
 }

 getGOChildren <- function(x) {
     if( !is.character(x) )
         stop("need a character argument")
     if(length(x) == 0 )
         return(list())
     hasMF <- mget(x, env=GOMFCHILDREN, ifnotfound=NA)
     hasBP <- mget(x, env=GOBPCHILDREN, ifnotfound=NA)
     hasCC <- mget(x, env=GOCCCHILDREN, ifnotfound=NA)
     lenx <- length(x)
     rval <- vector("list", length=lenx)
     names(rval) <- x
     for(i in 1:lenx) {
         if( (length(hasMF[[i]]) > 1 ) || !is.na(hasMF[[i]]) )
             rval[[i]] <- list(Ontology="MF", Children=hasMF[[i]])
         else if( (length(hasMF[[i]]) > 1 ) || !is.na(hasBP[[i]]) )
             rval[[i]] <- list(Ontology="BP", Children=hasBP[[i]])
         else if( (length(hasMF[[i]]) > 1 ) || !is.na(hasCC[[i]]) )
             rval[[i]] <- list(Ontology="CC", Children=hasCC[[i]])
         else
             rval[[i]] <- list()
     }
     return(rval)
 }

 getGOTerm <- function(x) {
     if( !is.character(x) )
         stop("need a character argument")
     if(length(x) == 0 )
         return(list())
     terms <- mget(x, env=GOTERM, ifnotfound=NA)
     ##cannot use is.na, because GOTerms objects are 0 length lists
     isNA = sapply(terms, function(x) !(is(x, "GOTerms")))
     if( any(isNA) )
         terms = terms[!isNA]

     ontology <- sapply(terms, Ontology)
     terms = sapply(terms, Term)
     return(split(terms, ontology))
 }


filterGOByOntology <- function(goids, ontology=c("BP", "CC", "MF")) {
    ontology <- match.arg(ontology)
    eName <- switch(ontology,
                    BP="GOBPPARENTS",
                    CC="GOCCPARENTS",
                    MF="GOMFPARENTS",
                    stop("invalid ontology ", ontology))
    e <- get(eName)
    goids %in% ls(e)
}
