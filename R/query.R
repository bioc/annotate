#Copyright 2001 R.Gentleman, all rights reserved
#functions to look up particular genes at different sites

openBrowser <- function(query) {
    OST <- .Platform$OS.type
    if( OST == "windows" ) {
        shell.exec(query)
    }
    else if( OST == "unix" ) {
        if (is.null(getOption("browser")))
            stop("options(\"browser\") not set")
        browser <- getOption("browser")
        system(paste(browser, " -remote \"openURL(",
                     query, ")\" 2>/dev/null || ", browser, " ",
                     query, " &", sep = ""))
    }
    else {
        msg <- paste("don't know how to open the browser on", OST)
        stop(msg)
    }
    return(NULL)
}

locuslink <- function(geneid, lladdress =
    "http://www.ncbi.nlm.nih.gov/LocusLink/") {
    if(is.na(geneid))
       stop("gene id is NA, cannot proceed")
    whichq <- "LocRpt.cgi?l="
    query <- paste(lladdress, whichq, geneid, sep="")
    openBrowser(query)
}

genbank <- function(geneid, gbaddress) {
    if(is.na(geneid))
       stop("gene id is NA, cannot proceed")
    if(missing(gbaddress))
       gbaddress <- "http://www.ncbi.nlm.nih.gov/"
    qname <- "htbin-post/Entrez/query?db=2&form=1&term="
    query <- paste(gbaddress, qname, geneid, sep="")
    openBrowser(query)
}

pubmed <- function(pmid, pmaddress) {
    if(is.na(pmid) )
        stop("pmid is NA, cannot proceed")
    if(missing(pmaddress))
        pmaddress <- "www.ncbi.nlm.nih.gov/entrez/"
    qname <-
    "query.fcgi?cmd=Retrieve&db=PubMed&dopt=DocSum&list_uids="
    query <- paste(pmaddress, qname, pmid, sep="")
    openBrowser(query)
}

genelocator <- function(x) {
  done<-FALSE
  while(!done) {
    v <- identify(x, n=1)
    if (length(v)==0)
      done <- TRUE
    else
      print(paste("hi I'm number", v))
  }
}

ll.htmlpage <- function (genelist, filename, title, othernames)
{
    outfile <- file(filename, "w")
    cat("<html>", "<head>", "<TITLE>BioConductor Gene Listing</TITLE>",
        "</head>", "<body bgcolor=#708090 >",
        "<H1 ALIGN=CENTER > BioConductor Gene Listing </H1>",
        file = outfile, sep = "\n")
    if( !missing(title) )
        cat("<CENTER><H1 ALIGN=\"CENTER\">", title, " </H1></CENTER>\n",
            file=outfile, sep = "\n")
    cat("<TABLE BORDER=4>", "<CAPTION> Locus Link Genes </CAPTION>",
        file = outfile, sep = "\n")
    rh <- "<TD> <A HREF=\"http://www.ncbi.nlm.nih.gov/LocusLink/LocRpt.cgi?l="
    nrows <- length(genelist)
    rows <- paste(rh, genelist, "\">", genelist, "</A> </TD>",
        sep = "")
    if( !missing(othernames) ) {
        if( is.list(othernames) ) {
            others <- ""
            for(nm in othernames)
                others <- paste(others,"<TD>", nm, "</TD>", sep="")
        }
        else
            others <- paste("<TD>", othernames, "</TD>", sep="")
        rows <- paste(rows, others)
    }
    for (i in 1:nrows)
        cat("<TR>", rows[i], "</TR>", file = outfile, sep = "\n")
    cat("</TABLE>", "</body>", "</html>", sep = "\n", file = outfile)
    close(outfile)
}


