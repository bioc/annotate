### chromLocation class

## Define the class structure of the chromLocation object
setGeneric("chromLocation", function(object)
           standardGeneric("chromLocation"))

setClass("chromLocation", representation(species="character",
                                         datSource="character",
                                         nChrom="numeric",
                                         chromNames="vector",
                                         chromLocs="list",
                                         chromLengths="vector",
                                         geneToChrom="environment"))


## Define the accessors
if (is.null(getGeneric("species")))
    setGeneric("species", function(object)
               standardGeneric("species"))

if (is.null(getGeneric("source")))
    setGeneric("datSource", function(object)
               standardGeneric("datSource"))

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

setMethod("show", "chromLocation", function(object) {
    cat("Instance of a chromLocation class with the following fields:\n")
    cat("\tSpecies: ", object@species, "\n\t")
    cat("Data source: ", object@datSource, "\n\t")
    cat("Number of chromosomes for this species: ", object@nChrom, "\n\t")

    ## Build up a matrix of chromosome names & their locations
    cat("Chromosomes of this species and their lengths in base pairs:")
    for (i in 1:object@nChrom) {
        cat("\n\t\t",object@chromNames[i],":",object@chromLengths[i])
    }
    cat("\n")
})


buildChromClass <- function(species, datSource, chromList,
                            chromLengths) {
    ## Passed a species name, the source of the data, a list which contains
    ## all the genes/location sorted by chromosome.  Will return a new
    ## instance of a chromLocation class, based on this information.

    ## Derive the other necessary information for this instantiation
    nChrom <- length(chromList)
    chromNames <- labels(chromList)
    chromEnv <- new.env(hash=TRUE)
    .fillGenEnv(chromList, chromEnv)

    ## INstantiate the new class and return it.
    newChroms <- new("chromLocation", species=species,
                     datSource=datSource, nChrom=nChrom,
                     chromNames=chromNames, chromLocs=chromList,
                     chromLengths=chromLengths,
                     geneToChrom=chromEnv)

    return(newChroms)
}

usedChromGenes <- function(eSet, chrom, specChrom) {
    ## Passed an instance of an exprSet, a chromosome name, and
    ## an instance of a chromLocation object - will return the
    ## set of genes in the eset that exist on the named chromosome.

    ## Extract the gene names of the chromosome of interest
    cLocs <- chromLocs(specChrom)
    genes <- cLocs[[chrom]]

    ## Extract out of the expr set the genes that belong on this chrom
    usedGenes <- genes[names(genes) %in% geneNames(eSet)]

    ## Order the genes by location
    usedGenes <- lapply(usedGenes, function(x){ord<-order(abs(x));x[ord]})

    return(usedGenes)
}


.fillGenEnv <- function(chromLocList, chromLocEnv) {
    ## Given a chromLocs list, will fill the chromLocEnv with the appropriate
    ## data

    for (i in 1:length(chromLocList)) {
        ## Convert the numeric name to the character name
        newName <- labels(chromLocList[i]);

        ## Get the gene names and location data
        chromData <- chromLocList[[i]]
        geneNames <- labels(chromData)
        newStrand <- vector(mode="character", length=length(geneNames))
        newPos <- as.numeric(chromData[geneNames])
        newStrand <- ifelse(newPos>0, "+", "-")
        newPos <- abs(newPos)

        newLoc <- list()

        for (j in 1:length(geneNames)) {
            ## Instantiate a new location object for this gene
            newLoc[[j]] <- new("chromLoc",
                               chrom=newName,position=newPos[j],
                               strand=newStrand[j])
        }
        multiassign(geneNames, newLoc, env=chromLocEnv)
    }
}
