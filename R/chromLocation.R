    # Defines the chromLocation class

    # Define the class structure of the chromLocation object
    setClass("chromLocation", representation(organism="character",
                                             dataSource="character",
                                             chromLocs="list",
                                             probesToChrom="environment",
                                             chromInfo="numeric",
                                             geneSymbols="environment"
                                             ))

    # Define the accessors
    if (is.null(getGeneric("organism")))
        setGeneric("organism", function(object)
                   standardGeneric("organism"))

    setMethod("organism", "chromLocation", function(object)
              object@organism)

    if (is.null(getGeneric("dataSource")))
        setGeneric("dataSource", function(object)
                   standardGeneric("dataSource"))

    setMethod("dataSource", "chromLocation", function(object)
              object@dataSource)

    if (is.null(getGeneric("nChrom")))
        setGeneric("nChrom", function(object)
                   standardGeneric("nChrom"))

    setMethod("nChrom", "chromLocation", function(object)
              length(object@chromInfo))

    if (is.null(getGeneric("chromNames")))
        setGeneric("chromNames", function(object)
                   standardGeneric("chromNames"))

    setMethod("chromNames", "chromLocation", function(object)
              names(object@chromInfo))

    if (is.null(getGeneric("chromLocs")))
        setGeneric("chromLocs", function(object)
                   standardGeneric("chromLocs"))

    setMethod("chromLocs", "chromLocation", function(object)
              object@chromLocs)

    if (is.null(getGeneric("chromLengths")))
        setGeneric("chromLengths", function(object)
                   standardGeneric("chromLengths"))

    setMethod("chromLengths", "chromLocation", function(object) {
        z <- as.numeric(object@chromInfo)
        ## Unknown chromosome lengths come out as NA from the
        ## data package, put this as 0 as we want a numeric vector
        z[is.na(z)] <- 0
        z
    })

    if (is.null(getGeneric("probesToChrom")))
        setGeneric("probesToChrom", function(object)
                   standardGeneric("probesToChrom"))

    setMethod("probesToChrom", "chromLocation", function(object)
              object@probesToChrom)

    if (is.null(getGeneric("chromInfo")))
        setGeneric("chromInfo", function(object)
                   standardGeneric("chromInfo"))
    setMethod("chromInfo", "chromLocation", function(object)
              object@chromInfo)

    if (is.null(getGeneric("geneSymbols")))
        setGeneric("geneSymbols", function(object)
                   standardGeneric("geneSymbols"))
    setMethod("geneSymbols", "chromLocation", function(object)
              object@geneSymbols)

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
y        }
        cat("\n")
    })


buildChromLocation <- function(dataPkg) {
    ##takes an environment/hash table with the chrom locations and
    ##named list, one element for each distinct chromosome name and
    ##each element of that list is a named vector, the names are the
    ##probeids and the values are the locations
    CHRLOC2chromLoc <- function(chrEnv) {
        chrLocs <- contents(chrEnv)

        ## Need to extract out the ones w/ multiple mappings
        chrLens <- sapply(chrLocs, length)
        multis <- split(chrLens, factor(chrLens))

        ## First handle the single mapped genes
        singleNames <- names(multis$"1")
        singleLocs <- chrLocs[singleNames]
        chromNames <- unlist(sapply(singleLocs, function(z) {
            if (is.na(z))
                z
            else
                names(z)
        }))
        chromNames <- factor(chromNames)
        a <- split(singleLocs, chromNames)
        chrLocList <- lapply(a, function(x) {g <- unlist(lapply(x, function(z)
                                                            {names(z) <- NULL;
                                                             z})); g})

        ## Now handle the multi mapped genes
        ## !!! FIXME:
        ## !!! This is *very* inefficient.  Make this better
        ## !!!
        if (length(multis) > 1) {
            for (i in 2:length(multis)) {
                curNames <- names(multis[[i]])
                curLocs <- chrLocs[curNames]
                for (j in 1:length(curLocs)) {
                    curGene <- curLocs[[j]]
                    curGeneChroms <- names(curGene)
                    names(curGene) <- rep(curNames[j],length(curGene))
                    for (k in 1:length(curGene))
                        chrLocList[[curGeneChroms[k]]] <-
                            c(chrLocList[[curGeneChroms[k]]], curGene[k])
                }
            }
        }


        chrLocList
    }

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


