.initChromLocation <- function() {
    # Defines the chromLocation class

    # Define the class structure of the chromLocation object
    setGeneric("chromLocation", function(object)
               standardGeneric("chromLocation"))

    setClass("chromLocation", representation(species="character",
             datSource="character", nChrom="numeric",
             chromNames="vector", chromLocs="list",
             chromLengths="vector", geneToChrom="environment"))


    # Define the accessors
    if (is.null(getGeneric("species")))
        setGeneric("species", function(object)
                   standardGeneric("species"))

    if (is.null(getGeneric("source")))
        setGeneric("datSource", function(object)
                   standardGeneric("source"))

    if (is.null(getGeneric("nChrom")))
        setGeneric("nChrom", function(object)
                   standardGeneric("nChrom"))

    if (is.null(getGeneric("chromNames")))
        setGeneric("chromNames", function(object)
                   standardGeneric("chromNames"))

    if (is.null(getGeneric("chromLocs")))
        setGeneric("chromLocs", function(object)
                   standardGeneric("chromLocs"))

    if (is.null(getGeneric("chromLengths")))
        setGeneric("chromLengths", function(object)
                   standardGeneric("chromLengths"))

    if (is.null(getGeneric("geneToChrom")))
        setGeneric("geneToChrom", function(object)
                   standardGeneric("geneToChrom"))

    setMethod("species", "chromLocation", function(object)
              object@species)

    setMethod("datSource", "chromLocation", function(object)
              object@datSource)

    setMethod("nChrom", "chromLocation", function(object)
              object@nChrom)

    setMethod("chromNames", "chromLocation", function(object)
              object@chromNames)

    setMethod("chromLocs", "chromLocation", function(object)
              object@chromLocs)

    setMethod("chromLengths", "chromLocation", function(object)
              object@chromLengths)

    setMethod("geneToChrom", "chromLocation", function(object)
              object@geneToChrom)
}

buildChromClass <- function(species, datSource, chromList,
                             chromLengths) {
# Passed a species name, the source of the data, a list which contains
# all the genes/location sorted by chromosome.  Will return a new
#instance of a chromLocation class, based on this information.

    # Derive the other necessary information for this instantiation
    nChrom <- length(chromList)
    chromNames <- labels(chromList)
    chromEnv <- new.env(hash=TRUE)
    .fillGenEnv(chromList, chromEnv)

    # INstantiate the new class and return it.
    newChroms <- new("chromLocation", species=species,
                      datSource=datSource, nChrom=nChrom,
                      chromNames=chromNames, chromLocs=chromList,
                      chromLengths=chromLengths,
                      geneToChrom=chromEnv)

    return(newChroms)
}

.fillGenEnv <- function(chromLocList, chromLocEnv) {
# Given a chromLocs list, will fill the chromLocEnv with the appropriate
# data

    for (i in 1:length(chromLocList)) {
        # Convert the numeric name to the character name
        newName <- labels(chromLocList[i]);

        # Get the gene names and location data
        chromData <- chromLocList[[i]]
        geneNames <- labels(chromData)
        newStrand <- vector(mode="character", length=length(geneNames))
        newPos <- as.numeric(chromData[geneNames])
        newStrand <- ifelse(newPos>0, "+", "-")
        newPos <- abs(newPos)

        for (j in 1:length(geneNames)) {

            # Instantiate a new location object for this gene
            newLoc <- new("chromLoc",
                          chrom=newName,position=newPos[j],
                          strand=newStrand[j])

            # Add the gene and chromLoc object to the environment
            assign(geneNames[j], newLoc, env=chromLocEnv)
        }
    }
}
