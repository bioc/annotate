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
        system(paste(browser, " -remote 'openURL(",
                     query, ")' 2>/dev/null || ", browser, " ",
                     query, " &", sep = ""))
    }
    else {
        msg <- paste("don't know how to open the browser on", OST)
        stop(msg)
    }
    return(NULL)
}

locuslinkQuery <- function(query,...,lladdress="LocusLink/") {
    if (is.na(query))
        stop("No query, cannot proceed!")

    if (length(c(...)) == 0) {
        species = "Hs"
    }
    else {
        species <- paste(...,sep="&ORG=")
    }


    ncbiURL <- .getNcbiURL()

    ## Build up the query URL

    query <- paste(ncbiURL, lladdress,
    "list.cgi?Q=",query,"&ORG=",species,"&V=0",sep="")

    openBrowser(query)
}

locuslinkByID <- function(..., lladdress="LocusLink/") {

    if (length(c(...)) == 0)
        stop("No Locuslink ID, cannot proceed")

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(...,sep=",")

    query <- paste(ncbiURL, lladdress, "LocRpt.cgi?l=", args, sep="")

    openBrowser(query)
}

genbank <- function(..., disp=c("data","browser")[1],
                    pmaddress=.pmfetch("Nucleotide",disp)) {
    if (length(c(...)) == 0)
        stop("No Gene ID, cannot proceed")

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(...,sep=",")

    id <- .getIdTag(disp)

    query <- paste(ncbiURL, pmaddress, id, args, sep="")

    ## Determine if we are displaying this data in a browser or
    ## returning an XMLDocument object
    if (disp == "data") {
        require(XML) || stop("XML package is unavailable!")
        return(xmlTreeParse(query,isURL=TRUE))
    }
    else {
        openBrowser(query)
    }
}

pubmed  <- function(..., disp=c("data","browser")[1],
                    pmaddress=.pmfetch("PubMed",disp)) {

    if (length(c(...)) == 0)
        stop("No PMID, cannot proceed")

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(...,sep=",")

    id <- .getIdTag(disp)

    query <- paste(ncbiURL, pmaddress, id, args, sep="")


    ## Determine if we are displaying this data in a browser or
    ## returning an XMLDocument object
    if (disp == "data") {
        require(XML) || stop("XML package is unavailable!")
        return(xmlTreeParse(query,isURL=TRUE))
    }
    else {
        openBrowser(query)
    }

}

.getNcbiURL <- function() {
    ## Returns the URL for NCBI, which should be located in Annotate's
    ## option set
    BioCOpt <- getOption("BioC")

    if (!is.null(BioCOpt)) {
        ncbiURL <- BioCOpt$Annotate$urls$ncbi
    }

    if (!exists("ncbiURL")) {
        ncbiURL <- "http://www.ncbi.nih.gov/"
    }

    return(ncbiURL)
}

.getIdTag <- function(disp=c("data","browser")[1]) {
    if (disp == "data") {
        return("&id=")
    }
    else {
        return("&list_uids=")
    }
}

.pmfetch <- function(db="PubMed", disp=c("data","browser")[1]) {
    ## Returns the base query string for the pmfetch engine @ pubmed



    if (disp == "data") {
        base <-
    "entrez/utils/pmfetch.fcgi?report=xml&mode=text&tool=bioconductor&db="
    }
    else {
        base <- "entrez/query.fcgi?cmd=Retrieve&tool=bioconductor&db="
    }
    return(paste(base,db,sep=""))
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


