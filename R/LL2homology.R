LL2homology <- function(llids){
    if(!require(homology)) stop("Package homology not available!")

    hgids <- mget(as.character(llids), env = homologyLL2HGID,
                  ifnotfound = NA)

    #if(length(hgids) == 1){
    #    return(HGID2homology(hgids[[1]]))
    #}

    return(sapply(hgids, HGID2homology))

}

ACC2homology <- function(accs){
    if(!require(homology)) stop("Package homology not available!")

    hgids <- mget(as.character(accs), env = homologyACC2HGID,
                  ifnotfound = NA)

    return(sapply(hgids, HGID2homology))
}

HGID2homology <- function(hgid){
    homoGenes <- list()

    #  hgid may be of length greater than 1 as a LL id may be mapped to
    # more than 2 HGIDs
    for(i in hgid){
        options(show.error.messages = FALSE)
        tryMe <- try(get(as.character(i),
                         env = get("homologyDATA",
                         pos = match("package:homology", search()))))
        options(show.error.messages = TRUE)
        if(!inherits(tryMe, "try-error")){
            homoGenes[[length(homoGenes) + 1]] <- tryMe
        }
    }
    return(homoGenes)
}
