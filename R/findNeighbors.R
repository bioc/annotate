findNeighbors <- function(chrLoc, llID,
                         chromosome, upBase, downBase, agreessive = TRUE){

    require(chrLoc, character.only = TRUE) ||
                           stop("Chromomosome location data unavailable!")
    # Find the location for the target gene
    location <- as.numeric(get(llID, get(paste(chrLoc,
                                      chromosome, "START", sep = ""))))
    if(agreessive){
        # agreessive search locat genes whose transcription either starts
        # (upstream) or ends (downstream) within the defined region.

        # Convert the environment to a matrix for searching
        uLoc <- cbind(ls(get(paste(chrLoc, chromosome, "START", sep = ""))),
                       unlist(multiget(ls(get(paste(chrLoc, chromosome,
                                             "START", sep = ""))),
                       get(paste(chrLoc, chromosome, "START", sep = "")))))
        dLoc <- cbind(ls(get(paste(chrLoc, chromosome, "END", sep = ""))),
                       unlist(multiget(ls(get(paste(chrLoc, chromosome,
                                             "END", sep = ""))),
                       get(paste(chrLoc, chromosome, "END", sep = "")))))
    }else{
        # non-agreessive search locat genes whose transcription either
        # ends (up stream) or start (down stream within the defined region.

        # Convert the environment to a matrix for searching
        uloc <- cbind(ls(get(paste(chrLoc, chromosome, "END", sep = ""))),
                       unlist(multiget(ls(get(paste(chrLoc, chromosome,
                                             "END", sep = ""))),
                       get(paste(chrLoc, chromosome, "END", sep = "")))))

        dloc <- cbind(ls(get(paste(chrLoc, chromosome, "START", sep = ""))),
                       unlist(multiget(ls(get(paste(chrLoc, chromosome,
                                             "START", sep = ""))),
                       get(paste(chrLoc, chromosome, "START", sep = "")))))
    }

    uNeighbor <- as.vector(uLoc[as.numeric(uLoc[,2]) <=
                          (location + as.numeric(upBase)) &
                           as.numeric(uLoc[,2]) > location, 1])
    dNeighbor <- as.vector(dLoc[as.numeric(dLoc[,2]) >=
                          (location - as.numeric(downBase)) &
                           as.numeric(dLoc[,2]) < location, 1])

    neighbors <- c(uNeighbor, dNeighbor)
    if(length(neighbors) == 0){
        return(NULL)
    }else{
        return(neighbors)
    }
}
