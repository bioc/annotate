#Copyright 2001 R.Gentleman, all rights reserved
#functions to look up particular genes at different sites

locuslink <- function(geneid, lladdress) {
    if(is.na(geneid))
       stop("gene id is NA, cannot proceed")
    if(missing(lladdress))
       lladdress <- "http://www.ncbi.nlm.nih.gov/LocusLink/LocRpt.cgi?l="
    query <- paste(lladdress, geneid, sep="")
    if (is.null(getOption("browser")))
       stop("options(\"browser\") not set")
    browser <- getOption("browser")
    system(paste(browser, " -remote \"openURL(",
              query, ")\" 2>/dev/null || ", browser, " ",
              query, " &", sep = ""))
    return(invisible())
}

genbank <- function(geneid, gbaddress) {
    if(is.na(geneid))
       stop("gene id is NA, cannot proceed")
    if(missing(gbaddress))
       gbaddress <- "http://www3.ncbi.nlm.nih.gov/"
    qname <- "htbin-post/Entrez/query?db=2&form=1&term="
    query <- paste(gbaddress, qname, geneid, sep="")
    if (is.null(getOption("browser")))
       stop("options(\"browser\") not set")
    browser <- getOption("browser")
    system(paste(browser, " -remote \"openURL(",
              query, ")\" 2>/dev/null || ", browser, " ",
              query, " &", sep = ""))
    return(invisible())
}
