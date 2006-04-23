##copyright 2004 R. Gentleman, all rights reserved

##given the name of chip compute the pathway adjacency matrix for LLids
PWAmat = function(data) {
    if(!is.character(data) || length(data) != 1 )
        stop("wrong argument")
    dataE = get(paste(data, "PATH2PROBE", sep=""))

    pathLL = eapply(dataE, function(x) {
        LLs = getLL(x, data)
        LLs = LLs[!is.na(LLs)]
        unique(LLs) })
    uniqLL = unique(unlist(pathLL,use.names=FALSE))
    Amat = sapply(pathLL, function(x) {
        mtch = match(x, uniqLL)
        zeros = rep(0, length(uniqLL))
        zeros[mtch] = 1
        zeros})
    dimnames(Amat) = list(uniqLL, names(pathLL))
    return(Amat)
}

##given the name of chip compute the PubMed adjacency matrix for probe set ids
PMIDAmat = function(pkg, gene=NULL) {
    if(!is.character(pkg) || length(pkg) != 1 )
        stop("wrong argument")

    probe2pmid <- get(paste(pkg, "PMID", sep=""))
    if(is.null(gene)){
        gene2pmid <- as.list(probe2pmid)
    }else{
        if(any(duplicated(gene))) warning("Gene is not unique.")
        gene2pmid <- mget(unique(gene), probe2pmid)
    }
    pmid <- unique(unlist(gene2pmid))

    Amat <- sapply(gene2pmid,
                   function(x){
                       mtch <- match(x, pmid)
                       zeros <- rep(0, length(pmid))
                       zeros[mtch] <- 1
                       return(zeros)
                   }
                   )
    dimnames(Amat) = list(pmid, names(gene2pmid))
    return(Amat)
}


##given a GO term, and an exprset, produce a heatmap of all probes
##mapped to that GOterm;
GO2heatmap = function(x, eset, data, ...) {
    if( missing(data) )
        data = eset@annotation
    mapE = get(paste(data, "GO2ALLPROBES", sep=""))

    whG = mapE[[x]]
    ##need this because there could be multiple criteria
    whG = unique(whG)
    whGs = whG[whG %in% geneNames(eset)]

    dataM = exprs(eset)[whGs,]
    heatmap(dataM, ...)
}

GOmnplot = function (x, eset, data = "hgu133plus2", group, ...)
{
    mapE = get(paste(data, "GO2ALLPROBES", sep = ""))
    whG = mapE[[x]]
    whG = unique(whG)
    whGs = whG[whG %in% geneNames(eset)]
    dataM = exprs(eset)[whGs, ]
    tts = apply(dataM, 1, function(x) sapply(split(x, group), mean))
    rn = row.names(tts)
    if( length(levels(factor(group))) != 2 )
        stop("only works for factors with two levels")
    plot(tts[1,], tts[2,], xlab=rn[1], ylab=rn[2], ...)
    abline(a=0, b=1)
    return(tts)
}

p2LL = function(data) {
    LLe = get(paste(data, "LOCUSID", sep=""))
    g1 = unlist(as.list(LLe))
    g2 = names(g1)
    names(g1) = NULL
    split(g2, g1)
}

KEGG2heatmap = function (x, eset, data, ...)
{   
    if( missing(data) )
       data = eset@annotation
    mapE = get(paste(data, "PATH2PROBE", sep = ""))
    whG = mapE[[x]]
    whG = unique(whG)
    whGs = whG[whG %in% geneNames(eset)]
    dataM = exprs(eset)[whGs, ] 
    heatmap(dataM, ...)
}


KEGGmnplot = function (x, eset, data = "hgu133plus2", group, ...)
{
    mapE = get(paste(data, "PATH2PROBE", sep = ""))
    whG = mapE[[x]]
    whG = unique(whG)
    whGs = whG[whG %in% geneNames(eset)]
    dataM = exprs(eset)[whGs, ]
    tts = apply(dataM, 1, function(x) sapply(split(x, group), mean))
    rn = row.names(tts)
    if( length(levels(factor(group))) != 2 )
        stop("only works for factors with two levels")
    plot(tts[1,], tts[2,], xlab=rn[1], ylab=rn[2], ...)
    abline(a=0, b=1)
    return(tts)
}

