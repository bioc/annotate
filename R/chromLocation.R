.initChromLocation <- function(where) {
    # Defines the chromLocation class

    # Define the class structure of the chromLocation object
    setClass("chromLocation", representation(organism="character",
                                             dataSource="character",
                                             chromLocs="list",
                                             probesToChrom="environment",
                                             chromInfo="numeric",
                                             geneSymbols="environment"
                                             ),where=where)

    # Define the accessors
    if (is.null(getGeneric("organism")))
        setGeneric("organism", function(object)
                   standardGeneric("organism"), where=where)

    setMethod("organism", "chromLocation", function(object)
              object@organism, where=where)

    if (is.null(getGeneric("dataSource")))
        setGeneric("dataSource", function(object)
                   standardGeneric("dataSource"), where=where)

    setMethod("dataSource", "chromLocation", function(object)
              object@dataSource, where=where)

    if (is.null(getGeneric("nChrom")))
        setGeneric("nChrom", function(object)
                   standardGeneric("nChrom"), where=where)

    setMethod("nChrom", "chromLocation", function(object)
              length(object@chromInfo), where=where)

    if (is.null(getGeneric("chromNames")))
        setGeneric("chromNames", function(object)
                   standardGeneric("chromNames"), where=where)

    setMethod("chromNames", "chromLocation", function(object)
              names(object@chromInfo), where=where)

    if (is.null(getGeneric("chromLocs")))
        setGeneric("chromLocs", function(object)
                   standardGeneric("chromLocs"), where=where)

    setMethod("chromLocs", "chromLocation", function(object)
              object@chromLocs, where=where)

    if (is.null(getGeneric("chromLengths")))
        setGeneric("chromLengths", function(object)
                   standardGeneric("chromLengths"), where=where)

    setMethod("chromLengths", "chromLocation", function(object) {
        z <- as.numeric(object@chromInfo)
        ## Unknown chromosome lengths come out as NA from the
        ## data package, put this as 0 as we want a numeric vector
        z[is.na(z)] <- 0
        z
    }, where=where)

    if (is.null(getGeneric("probesToChrom")))
        setGeneric("probesToChrom", function(object)
                   standardGeneric("probesToChrom"), where=where)

    setMethod("probesToChrom", "chromLocation", function(object)
              object@probesToChrom, where=where)

    if (is.null(getGeneric("chromInfo")))
        setGeneric("chromInfo", function(object)
                   standardGeneric("chromInfo"), where=where)
    setMethod("chromInfo", "chromLocation", function(object)
              object@chromInfo, where=where)

    if (is.null(getGeneric("geneSymbols")))
        setGeneric("geneSymbols", function(object)
                   standardGeneric("geneSymbols"), where=where)
    setMethod("geneSymbols", "chromLocation", function(object)
              object@geneSymbols, where=where)

    setMethod("show", "chromLocation", function(object) {
        cat("Instance of a chromLocation class with the following fields:\n")
        cat("\tOrganism: ", organism(object), "\n\t")
        cat("Data source: ", dataSource(object), "\n\t")
        cat("Number of chromosomes for this organism: ", nChrom(object), "\n\t")

        ## Build up a matrix of chromosome names & their locations
        cat("Chromosomes of this organism and their lengths in base pairs:")
        cNames <- chromNames(object)
        cLens <- chromLengths(object)
        for (i in 1:nChrom(object)) {
            cat("\n\t\t",cNames[i],":",cLens[i])
        }
        cat("\n")
    })
}

CHRLOC2chromLoc <- function(chrEnv) {
    chrLocs <- contents(chrEnv)

    chromNames <- unlist(sapply(chrLocs, function(y) {
        if (is.na(y))
            y
        else
            names(y)
    }))
    chromNames <- factor(chromNames)
    a <- split(chrLocs, chromNames)
    chrLocList <- lapply(a, function(x) {g <- unlist(lapply(x, function(y)
                                                        {names(y) <- NULL; y})); g})
    chrLocList
}

buildChromLocation <- function(dataPkg) {
    if (!require(dataPkg, character.only=TRUE))
        stop(paste("Package:",dataPkg,"is not available"))

    pEnv <- paste("package",dataPkg,sep=":")

    chrLocList <- CHRLOC2chromLoc(get(paste(dataPkg,"CHRLOC",sep=""), pos=pEnv))

    ## !!! Need to get the version info for dataSource
    newCC <- new("chromLocation",
                 organism=get(paste(dataPkg,"ORGANISM",sep=""),pos=pEnv),
                 dataSource=dataPkg,
                 chromLocs=chrLocList,
                 chromInfo=get(paste(dataPkg,"CHRLENGTHS",sep=""),pos=pEnv),
                 probesToChrom=get(paste(dataPkg,"CHR",sep=""),pos=pEnv),
                 geneSymbols=get(paste(dataPkg,"SYMBOL",sep=""),pos=pEnv))

    return(newCC)
}

usedChromGenes <- function(eSet, chrom, specChrom) {
    ## Passed an instance of an exprSet, a chromosome name, and
    ## an instance of a chromLocation object - will return the
    ## set of genes in the eset that exist on the named chromosome,
    ## ordered by location

    ## Extract the gene names of the chromosome of interest
    cLocs <- chromLocs(specChrom)
    genes <- cLocs[[chrom]]

    ## Extract out of the expr set the genes that belong on this chrom
    usedGenes <- genes[names(genes) %in% geneNames(eSet)]

    ## Order the genes by location
    ord <- order(abs(usedGenes))
    usedGenes <- as.list(usedGenes[ord])

    return(usedGenes)
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
