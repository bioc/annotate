# This function takes a character string for the name of a platform
# specific annotation data package and then returns a list of vectors
# with LocusLink ids as names and vectors of character strings for
# names of probes associated with the LocusLink ids as values.
#
# baseName - a character string for the name of a platform specific
# annotation data package (e. g. for an Affymetrix chip).
#
# Copyright 2004, Jianhua Zhang. All rights reserved.
#

probesByLL <- function(baseName){

    require(baseName, character.only = TRUE) ||
    stop(paste("The annotation package for", chipName, "is not available!"))

    temp <- unlist(lookUp(ls(get(paste(baseName, "LOCUSID", sep = ""))),
                      baseName, "LOCUSID"))
    temp <- temp[!is.na(temp)]
    temp <- split.data.frame(cbind(temp, names(temp)), factor(temp))

    return(sapply(temp, FUN = function(x) as.vector(x[,2])))
}
