findNeighbors <- function(chrLoc, llID, chromosome, upBase, downBase,
                          mergeOrNot = TRUE){

    require(chrLoc, character.only = TRUE) ||
                           stop(paste("Chromomosome location chrLoc",
                                      "is not available on the system",
                                      "Either build one or get one from",
                                      "BioConductor"))

    if(checkArgs(llID, chromosome, upBase, downBase) == "swap"){
        temp <- upBase
        upBase <- downBase
        downBase <- temp
    }
    upBase <- as.numeric(ifelse(missing(upBase), 0, upBase))
    downBase <- as.numeric(ifelse(missing(downBase), 0, downBase))
    if(missing(chromosome)){
        chromosome <- findChr4LL(llID, get(paste(chrLoc,
                                                 "LOCUSID2CHR", sep = "")),
                                gsub("CHRLOC", "", chrLoc))
    }
    if(!missing(llID)){
        # Find the location for the target gene
        location <- as.numeric(get(llID, get(paste(chrLoc,
                                      chromosome, "START", sep = ""))))
    }else{
        location <- (downBase - upBase)/2
    }
    upperB <- getBoundary(location, upBase, TRUE)
    downB <- getBoundary(location, downBase, FALSE)
    neighbors <- list()
    # There may be chances that a llID be mapped to genes on different CHR
    for(i in chromosome){
        start <- unlist(multiget(ls(get(paste(chrLoc, chromosome,
                                              "START", sep = ""))),
                                 get(paste(chrLoc, chromosome,
                                           "START", sep = ""))),
                        use.names = TRUE)
        end <- unlist(multiget(ls(get(paste(chrLoc, chromosome,
                                              "END", sep = ""))),
                                 get(paste(chrLoc, chromosome,
                                           "END", sep = ""))),
                        use.names = TRUE)
        if(!missing(llID)){
            # greb the ones in the range
            foundUp <- weightByConfi(start[start > upperB &
                                           start < location])
            foundDown <- weightByConfi(end[end < downB &
                                           end > location])
            if(length(foundUp) != 0 || length(foundDown) != 0){
                if(mergeOrNot){
                    neighbors[[as.character(i)]] <- unique(c(foundUp,
                                                             foundDown))
                }else{
                    neighbors[[as.character(i)]] <-
                        list(upstream = foundUp, downstream = foundDown)
                }
            }
        }else{
            found <- weightByConfi(c(start[start >= upperB &
                                           start <= location],
                                     end[end <= downB & end >= location]))
            if(length(found) != 0){
                 neighbors[[as.character(i)]] <- unique(found)
            }
        }
    }

    if(length(neighbors) == 0){
        warning("No Genes in the defined region satisfy the condition")
    }else{
        return(neighbors)
    }
}

checkArgs <- function(llID, chromosome, upBase, downBase){
    # llID is not required if search for genes within a range
    if(missing(llID)){
        # Both upBase, downBase, and chromosome must be there if
        # llID is missing
        if(any(missing(upBase), missing(downBase), missing(chromosome))){
            stop(paste("Search can not be conducted with llID and",
                       "at least one of upBase, downBase and ",
                       "chromosome missing"))
        }else{
            if(as.numeric(upBase) < as.numeric(downBase)){
                warning(paste("upBase value is smaller then downBase",
                              "value. Values have been swapped"))
                return("swap")
            }
            if(as.numeric(upBase) == as.numeric(downBase)){
                stop("upBase and downBase can not be the same")
            }
        }
    }else{
        if(missing(upBase)){
            warning(paste("upBase is missing. Search will be",
                          "conducted for genes downstream only"))
        }
        if(missing(downBase)){
            warning(paste("downBase is missing. Search will be",
                          "conducted for genes upstream only"))
        }
    }
    return("OK")
}

findChr4LL <- function(llID, chrEnv, organism){
    options(show.error.message = FALSE)
    chr <- try(get(llID, chrEnv))
    options(show.error.message = TRUE)
    if(inherits(chr, "try-error")){
        stop(paste("LocusLink id", llID, "could not be found in any",
                   "of the chromosomes in the data package"))
    }else{
        if(length(chr) == 1){
            if(!is.element(chr, getValidChr(organism))){
                stop(paste("LocusLink id", llID, "is currently",
                           "not known to be associated with any",
                           "chromosome"))
            }
            return(chr)
        }else{
            chr <- chr[is.element(chr, getValidChr(organism))]
            return(unique(chr))
        }
    }
}


getValidChr <- function(organism){
    switch(toupper(organism),
           HUMAN = return(c(1:22, "X", "Y")),
           MOUSE = return(c(1:19, "X", "Y")),
           RAT = return(c(1:20, "X", "Y")),
           stop(paste("Unknow organism", organism)))
}

getBoundary <- function(loc, base, lower = TRUE){
    if(as.numeric(loc) == 0){
        return(base)
    }else{
        if(lower){
            boundary <- as.numeric(loc) - base
            if(boundary < 0){
                return(0)
            }else{
                return(boundary)
            }
        }else{
            return(as.numeric(loc) + base)
        }
    }
}

weightByConfi <- function(foundLLs){
    if(length(foundLLs) != 0){
        temp <- unique(names(foundLLs))
        foundLLs <- gsub("(^.*)\\..*", "\\1", temp)
        names(foundLLs) <- gsub("^.*\\.(.*)", "\\1", temp)
        # Remove LLs named Unconfident if one named Confident exists
        if(any(duplicated(foundLLs))){
            foundLLs <- c(foundLLs[names(foundLLs) == "Confident"],
                          foundLLs[names(foundLLs) != "Confident"])
            foundLLs <- foundLLs[!duplicated(foundLLs)]
        }

        return(foundLLs)
    }else{
        return("")
    }
}
