.initChromLocation <- function() {
    # Defines the chromLocation class and creates its associated environment

    # Define the environment object used in the class
    chromLocEnv <- new.env(hash=TRUE)

    # Define the class structure of the chromLocation object
    setGeneric("chromLocation", function(object)
               standardGeneric("chromLocation"))

    setClass("chromLocation", representation(species="character",
             datSource="character", nChrom="numeric",
             chromNames="vector", chromLocs="list",
             chromLengths="vector", geneToChrom="chromLocEnv"))


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

fillGenEnv <- function(chromLocList) {
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
        ifelse(newPos>0, newStrand <- "+", newStrand <- "-")
        newPos <- abs(newPos)

        for (j in 1:length(geneNames)) {

            # Instantiate a new location object for this gene
            newLoc <- new("chromLoc",
                          chrom=newName[j],position=newPos[j],
                          strand=newStrand[j])

            # Add the gene and chromLoc object to the environment
            assign(geneNames[j], newLoc, env=chromLocEnv)
        }
    }
}
