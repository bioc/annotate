# This function takes a character string for the name of the annotation
# package for an Affymetrix chip and then returns a list of vectors
# with LocusLink ids as names and vectors of character strings for
# names of probes contained by the chip and belong to the LocusLink ids.
#
# chipName - a character string for the name of an annotation data
# package for a given Affymetrix chip.
#
# Copyright 2004, Jianhua Zhang. All rights reserved.
#

probesByLL <- function(chipName){

    require(chipName, character.only = TRUE) ||
    stop(paste("The annotation package for", chipName, "is not available!"))

    temp <- unlist(lookUp(ls(get(paste(chipName, "LOCUSID", sep = ""))),
                      chipName, "LOCUSID"))
    temp <- temp[!is.na(temp)]
    temp <- split.data.frame(cbind(temp, names(temp)), factor(temp))

    return(sapply(temp, FUN = function(x) as.vector(x[,2])))
}
