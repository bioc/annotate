ACCNUMStats <- function(pkgName){
    if(!require(pkgName, character.only = TRUE)){
        stop(paste("Data package", pkgName, "is not available"))
    }
    accs <- as.list(get(paste(pkgName, "ACCNUM", sep = "")),
                    pos = match(paste("package:", pkgName, sep = ""),
                    search()) )

    return(table(unlist(sapply(accs, whatACC))))
}

whatACC <- function(accs){
    if(is.na(accs[1])){
        return(NA)
    }
    accs <- strsplit(accs, ";")
    if(regexpr("^[a-zA-Z]{2}\\.[0-9]+$", accs[1]) > 0){
        return("UniGene")
    }
    if(regexpr("^(NP_)|(NG_)|(NM_)|(NC_)|(XR_)|(XM_)|(XP_)[0-9]+$",
               accs[1]) > 0){
        return("RefSeq")
    }
    if(regexpr("^[a-zA-Z]+[0-9]+$", accs[1]) > 0){
        return("GBAcc")
    }
    if(regexpr("^[0-9]+$", accs[1]) > 0){
        return("Image")
    }

    return(NA)
}

