##copyright 2004 R. Gentleman, all rights reserved

##given the name of chip compute the pathway adjacency matrix for LLids
PWAmat = function(data) {
    if(!is.character(data) || length(data) != 1 )
        stop("wrong argument")
    dataE = get(paste(data, "PATH2PROBE", sep=""))

    pathLL = eapply(dataE, function(x) {
        LLs = getLL(x, "hgu133plus2")
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

##given a GO term, and an exprset, produce a heatmap of all probes
##mapped to that GOterm;
GO2heatmap = function(x, eset, data="hgu133plus2", ...) {
    mapE = get(paste(data, "GO2ALLPROBES", sep=""))

    whG = mapE[[x]]
    ##need this because there could be multiple criteria
    whG = unique(whG)
    whGs = whG[whG %in% geneNames(eset)]

    dataM = exprs(eset)[whGs,]
    heatmap(dataM, ...)
    return(dataM)
}

p2LL = function(data) {
    LLe = get(paste(data, "LOCUSID", sep=""))
    g1 = unlist(as.list(LLe))
    g2 = names(g1)
    names(g1) = NULL
    split(g2, g1)
}

KEGG2heatmap = function (x, eset, data = "hgu133plus2", ...)
{   
    mapE = get(paste(data, "PATH2PROBE", sep = ""))
    whG = mapE[[x]]
    whG = unique(whG)
    whGs = whG[whG %in% geneNames(eset)]
    dataM = exprs(eset)[whGs, ] 
    heatmap(dataM, ...)
    return(dataM)
}


