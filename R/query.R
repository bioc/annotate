#Copyright 2001 R.Gentleman, all rights reserved
#functions to look up particular genes at different sites

UniGeneQuery <- function(query, UGaddress="UniGene/",
                         type="CID") {
    if (missing(query))
        stop("No query, cannot proceed!")

    ##they are of the form HH.xxxx, where HH specifies the species
    q1 <- strsplit(query, "\\.")
    if( length(q1[[1]]) == 2 ) {
        id <- sapply(q1, function(x) x[2])
        species <- sapply(q1, function(x) x[1])
    }

    ncbiURL <- .getNcbiURL()
    ## Build up the query URL

    query <- paste(ncbiURL, UGaddress,
    "clust.cgi?ORG=",species,"&", type, "=",id, sep="")

    return(query)
}

pmidQuery <- function(query) {
    if (missing(query))
        stop("No query, cannot proceed!")

    query <- paste(query,collapse="%2c")
    ncbiURL <- .getNcbiURL()

    query <- paste(ncbiURL,"/entrez/query.fcgi?cmd=Retrieve&db=PubMed&",
                 "list_uids=",query,"&dopt=Abstract&tool=bioconductor",sep="")

    return(query)
}

locuslinkQuery <- function(query,...,lladdress="LocusLink/", browse=TRUE) {
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

    if (browse)
        browseURL(query)
    else
        return(query)
}

locuslinkByID <- function(..., lladdress="LocusLink/", browse=TRUE) {
    params <- list(...)
    params <- unlist(params)

    if (length(params) == 0)
        stop("No Locuslink ID, cannot proceed")

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(params,collapse="%2c")

    query <- paste(ncbiURL, lladdress, "LocRpt.cgi?l=", args, sep="")

    if (browse)
        browseURL(query)
    else
        return(query)
}

genbank <- function(..., disp=c("data","browser"),
                    type=c("accession", "uid"),
                    pmaddress=.pmfetch("Nucleotide",disp,type)) {
    params <- list(...)
    params <- unlist(params)

    disp <- match.arg(disp)
    type <- match.arg(type)

    if (length(params) == 0) {
        stop("No Gene ID, cannot proceed")
    }

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(params,collapse="%2c")
    ## See if we need to transform accession based arguments
    err <- args
    args <- .transformAccession(args, disp, type,db="genbank")

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
        browseURL(query)
    }
}

pubmed  <- function(..., disp=c("data","browser"),
                    type=c("uid","accession"),
                    pmaddress=.pmfetch("PubMed",disp,type)) {
    params <- list(...)
    params <- unlist(params)

    disp <- match.arg(disp)
    type <- match.arg(type)

    if (length(params) == 0) {
        stop("No PMID, cannot proceed")
    }

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(params,collapse="%2c")
    ## See if we need to transform accession based arguments
    err <- args
    args <- .transformAccession(args, disp, type,"pubmed")

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
        browseURL(query)
    }
}

accessionToUID <- function(...,db=c("genbank","pubmed")) {
    ## Passed an accession #, returns a pubmed UID

    accNum <- list(...)
    accNum <- unlist(accNum)
    accNum <- paste(accNum,collapse="+OR+")

    db <- match.arg(db)

    ## Certain functions will be passing in a single string of comma
    ## deliminated Accession #s.  Change the commas to "+OR+"
    accNum <- gsub("\\,","+OR+",accNum)

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
    ## Make sure that XML version is what we require
    ## !!! Need to make this automatic, hardcode version in for now
    xmlVers <- package.description("XML",fields="Version")
    reqXmlVers <- "0.92-2"
    if (compareVersion(xmlVers,reqXmlVers) < 0)
        stop(paste("Installed XML version is ",xmlVers,
                   " while this functionality requires ", reqXmlVers,
                   ":  Please update your XML package.",sep=""))

    options(show.error.messages = FALSE)
    on.exit(options(show.error.messages = TRUE))
    retVal <- NULL
    result <- try(xmlTreeParse(query,isURL=TRUE, handlers=list(Id=function(x,attrs) {retVal <<- xmlValue(x[[1]])})))
    options(show.error.messages = TRUE)

    if (!is.null(retVal)) {
        ## In the event of multiple IDs, it returns as a monolithic
        ## which is space delimited.  Change this to comma deliminated
        retVal <- gsub(" *", "\\,", retVal)
    }

    return(retVal)
}


.handleXML <- function(query,handlers=NULL) {
    ## In the case of an error retrieving proper XML output,
    ## will return NA to the calling function
    require(XML) || stop("Sorry, you need the XML package!")
    ## Make sure that XML version is what we require
    ## !!! Need to make this automatic, hardcode version in for now
    xmlVers <- package.description("XML",fields="Version")
    reqXmlVers <- "0.92-2"
    if (compareVersion(xmlVers,reqXmlVers) < 0)
        stop(paste("Installed XML version is ",xmlVers,
                   " while this functionality requires ", reqXmlVers,
                   ":  Please update your XML package.",sep=""))

    options(show.error.messages = FALSE)
    on.exit(options(show.error.messages = TRUE))
    retVal <- NULL
    xml <- try(xmlTreeParse(query,isURL=TRUE,handlers=NULL,asTree=TRUE))
    options(show.error.messages = TRUE)

    if (inherits(xml,"try-error") == TRUE) {
        return(NA)
    }

    return(xml)
}

.getNcbiURL <- function() {
    ## Returns the URL for NCBI, which should be located in Annotate's
    ## option set
    BioCOpt <- getOption("BioC")

    if (!is.null(BioCOpt)) {
        ncbiURL <- BioCOpt$annotate$urls$ncbi
    }

    if (!exists("ncbiURL")) {
        ncbiURL <- "http://www.ncbi.nih.gov/"
    }

    return(ncbiURL)
}

.getIdTag <- function(disp=c("data","browser"),
                      type=c("uid","accession")) {
    disp <- match.arg(disp)
    type <- match.arg(type)

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

.pmfetch <- function(db="PubMed", disp=c("data","browser"),
                     type=c("uid","accession")) {
    ## Returns the base query string for the pmfetch engine @ pubmed

    disp <- match.arg(disp)
    type <- match.arg(type)

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

.transformAccession <- function(args, disp, type, db) {
    ## Used to change accession ID arguments to query functions
    ## into UIDs if necessary.  Returns NULL if there aren't any left.
    if ((disp == "data")&&(type=="accession")) {
        args <- accessionToUID(args,db=db)
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

pmAbst2html <- function(absts, filename, title, simple=TRUE,
                      table.center=TRUE) {
    ## Currently just a very naive implementation of a pmid2html type
    ## of thing.  Intended to be temporary just while I'm testing some
    ## of this stuff.

    if (!is.list(absts)) {
        if (is(absts,"pubMedAbst"))
            absts <- list(absts)
        else
            stop("'absts' parameter does not seem to be valid.")
    }

    if (missing(filename))
        filename <- "absts.html"

    outfile <- file(filename,"w")
    cat("<html>", "<head>", "<TITLE>BioConductor Abstract List</TITLE>",
        "</head>", "<body bgcolor=#708090 >",
        "<H1 ALIGN=CENTER > BioConductor Abstract List </H1>",
        file = outfile, sep = "\n")
    if ( !missing(title) )
        cat("<CENTER><H1 ALIGN=\"CENTER\">", title, " </H1></CENTER>\n",
            file=outfile, sep = "\n")
    if( table.center )
        cat("<CENTER> \n", file=outfile)

    cat("<TABLE BORDER=1>", file = outfile, sep = "\n")
    head <- c("Article Title", "Publication Date")
    headOut <- paste("<TH>", head, "</TH>")
    cat("<TR>",headOut,"</TR>", file=outfile, sep="\n")

    nrows = length(absts)
    pmids <- unlist(lapply(absts,pmid))
    dates <- unlist(lapply(absts,pubDate))
    queries <- unlist(lapply(absts,
                             function(x){pm <- pmid(x);out<-pmidQuery(pm);out}))
    titles <- unlist(lapply(absts, articleTitle))
    anchors <- makeAnchor(queries, titles)
    tds <- paste("<TD>",anchors,"</TD><TD>",dates,"</TD>",sep="")
    for (td in tds)
        cat("<TR>", td, "</TR>", file=outfile,sep="\n")

    cat("</TABLE>",file=outfile)
    if( table.center )
        cat("</CENTER> \n", file=outfile)
    cat("</body>", "</html>", sep = "\n", file = outfile)
    close(outfile)
}

ll.htmlpage <- function (genelist, filename, title, othernames,
                         table.head, table.center=TRUE,
                         repository = "ug")
{
    outfile <- file(filename, "w")
    cat("<html>", "<head>", "<TITLE>BioConductor Linkage List</TITLE>",
        "</head>", "<body bgcolor=#708090 >",
        "<H1 ALIGN=CENTER > BioConductor Linkage List </H1>",
        file = outfile, sep = "\n")
    if( !missing(title) )
        cat("<CENTER><H1 ALIGN=\"CENTER\">", title, " </H1></CENTER>\n",
            file=outfile, sep = "\n")

    if( table.center )
        cat("<CENTER> \n", file=outfile)

    cat("<TABLE BORDER=4>", file = outfile, sep = "\n")
    if( !missing(table.head) ) {
        headout <- paste("<TH>", table.head, "</TH>")
        cat("<TR>", headout, "</TR>", file=outfile, sep="\n")
    }
#    rh <- "<TD> <A HREF=\"http://www.ncbi.nlm.nih.gov/LocusLink/LocRpt.cgi?l="
    nrows <- length(genelist)
    rows <- getTDRows(genelist, repository)
#    rows <- paste(rh, genelist, "\">", genelist, "</A> </TD>",
#        sep = "")
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
    cat("</TABLE>",file=outfile)
    if( table.center )
        cat("</CENTER> \n", file=outfile)
    cat("</body>", "</html>", sep = "\n", file = outfile)

    close(outfile)
}

getTDRows <- function(ids, repository = "ug"){
    paste("<TD> <A HREF=\"", getQueryLink(ids, repository),
          "\">", ids, "</A> </TD>", sep = "")
}

getQueryLink <- function(ids, repository = "ug"){
    switch(tolower(repository),
           "ug" = return(getQuery4UG(ids)),
           "ll" = return(getQuery4LL(ids)),
           stop("Unknown repository name"))
}

getQuery4UG <- function(ids){
    # UG ids = XX.yyyy. Split by "."
    temp <- matrix(unlist(strsplit(ids, "\\."), use.names = FALSE),
                   ncol = 2, byrow = TRUE)
    paste("http://www.ncbi.nlm.nih.gov/UniGene/clust.cgi?ORG=",
              temp[,1], "&CID=", temp[,2], sep = "")
}

getQuery4LL <- function(ids){
    paste("http://www.ncbi.nlm.nih.gov/LocusLink/LocRpt.cgi?l=",
          ids, sep = "")
}

