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
    params <- list(...)
    params <- unlist(params,use.names=FALSE)

    if (is.na(query))
        stop("No query, cannot proceed!")

    if (length(c(params)) == 0) {
        species = "Hs"
    }
    else {
        species <- paste(params,collapse="&ORG=")
    }


    ncbiURL <- .getNcbiURL()

    ## Build up the query URL

    query <- paste(ncbiURL, lladdress,
    "list.cgi?Q=",query,"&ORG=",species,"&V=0",sep="")

    openBrowser(query)
}

locuslinkByID <- function(..., lladdress="LocusLink/") {
    params <- list(...)
    params <- unlist(params)

    if (length(params) == 0)
        stop("No Locuslink ID, cannot proceed")

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(params,collapse=",")

    query <- paste(ncbiURL, lladdress, "LocRpt.cgi?l=", args, sep="")

    openBrowser(query)
}

genbank <- function(..., disp=c("data","browser")[1],
                    type=c("uid","accession")[2],
                    pmaddress=.pmfetch("Nucleotide",disp,type)) {
    params <- list(...)
    params <- unlist(params)

    if (length(params) == 0) {
        stop("No Gene ID, cannot proceed")
    }

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(params,collapse=",")
    ## See if we need to transform accession based arguments
    err <- args
    args <- .transformAccession(args, disp, type)

   if (is.null(args)) {
        print(paste("No XML records available for accession number",err))
        return(NULL)
    }

    id <- .getIdTag(disp,type)

    query <- paste(ncbiURL, pmaddress, id, args, sep="")

    ## Determine if we are displaying this data in a browser or
    ## returning an XMLDocument object
    if (disp == "data") {
        return(.handleXML(query))
    }
    else {
        openBrowser(query)
    }
}

pubmed  <- function(..., disp=c("data","browser")[1],
                    type=c("uid","accession")[1],
                    pmaddress=.pmfetch("PubMed",disp,type)) {
    params <- list(...)
    params <- unlist(params)

    if (length(params) == 0) {
        stop("No PMID, cannot proceed")
    }
    else if (disp == "data") {
        params <- accessionToUID(params,"pubmed")
    }

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(params,collapse=",")
    ## See if we need to transform accession based arguments
    err <- args
    args <- .transformAccession(args, disp, type)

    if (is.null(args)) {
        print(paste("No XML records available for accession number",err))
        return(NULL)
    }


    id <- .getIdTag(disp,type)

    query <- paste(ncbiURL, pmaddress, id, args, sep="")


    ## Determine if we are displaying this data in a browser or
    ## returning an XMLDocument object
    if (disp == "data") {
        return(.handleXML(query))
    }
    else {
        openBrowser(query)
    }
}

accessionToUID <- function(accNum,db=c("genbank","pubmed")[1]) {
    ## Passed an accession #, returns a pubmed UID

    if (db == "genbank") {
        db <- "nucleotide"
    }
    else {
        db <- "PubMed"
    }

    query <- paste(.getNcbiURL(), "entrez/utils/pmqty.fcgi?db=", db,
                   "&tool=bioconductor&mode=xml&term=",accNum,sep="")

    ## Currently doubling up on code from .handleXML as I can't yet find a
    ## way to retrieve values back through the extra layer of
    ## indirection.

    require(XML) || stop("XML package is unavailable!")
    options(show.error.messages = FALSE)
    on.exit(options(show.error.messages = TRUE))
    retVal <- NULL
    result <- try(xmlTreeParse(query,isURL=TRUE,handlers=
                               list(Id=function(x,attrs) {retVal <<- xmlValue(x[[1]])})))
    options(show.error.messages = TRUE)

    if (!is.null(retVal)) {
        ## In the event of multiple IDs, it returns as a monolithic
        ## which is space delimited.  Change this to comma deliminated
        retVal <- gsub(" *", "\\,", retVal)
    }

    return(retVal)
}


.handleXML <- function(query,handlers=NULL) {
    require(XML) || stop("XML package is unavailable!")
    options(show.error.messages = FALSE)
    on.exit(options(show.error.messages = TRUE))
    retVal <- NULL
    xml <- try(xmlTreeParse(query,isURL=TRUE,handlers=NULL,asTree=TRUE))
    options(show.error.messages = TRUE)

    if (inherits(xml,"try-error") == TRUE) {
        print("Could not retrieve XML data, please check your settings.")
        print("Returning an object of class try-error")
    }

    return(xml)
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

.getIdTag <- function(disp=c("data","browser")[1],
                      type=c("uid","accession")[1]) {
    if (disp == "data") {
        return("&id=")
    }
    else {
        if (type == "uid") {
            return("&list_uids=")
        }
        else {
            return("&term=")
        }
    }
}

.pmfetch <- function(db="PubMed", disp=c("data","browser")[1],
                     type=c("uid","accession")[1]) {
    ## Returns the base query string for the pmfetch engine @ pubmed

    if (disp == "data") {
        base <-
    "entrez/utils/pmfetch.fcgi?report=xml&mode=text&tool=bioconductor&db="
    }
    else {
        base1 <- "entrez/query.fcgi?tool=bioconductor&cmd="
        if (type == "uid") {
            base2 <- "Retrieve&db="
        }
        else {
            base2 <- "Search&db="
        }
        base <- paste(base1,base2,sep="")
    }
    return(paste(base,db,sep=""))
}

.transformAccession <- function(args, disp, type) {
    ## Used to change accession ID arguments to query functions
    ## into UIDs if necessary.  Returns NULL if there aren't any left.
    if ((disp == "data")&&(type=="accession")) {
        err <- args
        args <- accessionToUID(args)
    }

    return(args)
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


