#Copyright 2001 R.Gentleman, all rights reserved
#functions to look up particular genes at different sites
# Modifications to htmlpage and getQuery4XX functions added
# 7-12-04 by J. MacDonald

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

    if (is.na(query) || length(query) == 0 )
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
    return(query)
}

locuslinkByID <- function(..., lladdress="LocusLink/", browse=TRUE) {
    params <- list(...)
    params <- unlist(params)

    if (length(params) == 0)
        stop("No Locuslink ID, cannot proceed")

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    if (length(params) == 1) {
        args <- paste(params,collapse="%2c")
        query <- paste(ncbiURL, lladdress, "LocRpt.cgi?l=", args, sep="")
    }
    else {
        args <- paste(params,collapse="&ID=")
        query <- paste(ncbiURL, lladdress, "list.cgi?ID=", args, sep="")
    }

    if (browse)
        browseURL(query)
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
    xmlVers <- packageDescription("XML",fields="Version")
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
    xmlVers <- packageDescription("XML",fields="Version")
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


pmAbst2HTML <- function(absts, filename, title, frames = FALSE,
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

    ## Assign a default filename.  If we're using frames, then
    ## 'filename' is really just the base filename, so make it empty
    if (missing(filename))
        if (frames)
            fileName <- ""
        else
            filename <- "absts.html"

    if (missing(title))
        title <- "BioConductor Abstract List"

    nrows = length(absts)
    pmids <- unlist(lapply(absts,pmid))
    dates <- unlist(lapply(absts,pubDate))
    queries <- unlist(lapply(absts,
                             function(x){pm <- pmid(x);out<-pmidQuery(pm);out}))
    titles <- unlist(lapply(absts, articleTitle))
    ## If we're using frames, need to point the anchors to
    ## the main frame, otherwise not.
    anchors <- makeAnchor(queries, titles, toMain=frames)

    topText <- paste("<html>\n<head>\n<title>", title, "</title>",
                     "\n</head>\n<body bgcolor=#708090>\n",
                     "<H1 ALIGN=CENTER>", title, "</H1>\n",
                     "</body></title>", sep="")
    head <- c("Article Title", "Publication Date")
    headOut <- paste("<TH>", head, "</TH>", collapse="\n")

    if (frames) {
        top <- new("HTMLPage", fileName=paste(filename,"Top.html",sep=""),
                   pageText= topText)
        tableHeader <- paste("<TR>",headOut,"</TR>", sep="\n")
        sideText <- paste("<TABLE BORDER=1>", tableHeader, sep="\n")

        tds <- paste("<TD>",anchors,"</TD><TD>",dates,"</TD>",sep="",
                     collapse="\n</TR>\n<TR>\n")
        tds <- paste("<TR>",tds,"</TR>")
        sideText <- paste(sideText, tds)
        if (table.center)
            sideText <- paste("<CENTER>",sideText,"</CENTER>", sep="\n")
        sideText <- paste("<html>", "<head>",
                          "<title>BioConductor Abstract List</title>",
                          "</head>","<body bgcolor=#708090>",
                          sideText, "</body>", "</html>", sep="\n")
        side <- new("HTMLPage",
                    fileName=paste(filename,"Side.html",sep=""),
                    pageText=sideText)

        metaText <- paste("<meta HTTP-EQUIV=\"REFRESH\" CONTENT=\"1;",
                          queries[1],"\">",sep="")
        mainText <- paste("<html>", "<head>",
                          "<title>BioConductor Abstract List</title>",
                          "</head>","<body bgcolor=#708090>",
                          metaText,
                          "</body>","</html>", sep="\n")

        main <- new("HTMLPage",
                    fileName=paste(filename,"Main.html",sep=""),
                    pageText=mainText)

        page <- new("FramedHTMLPage", topPage=top, sidePage=side, mainPage=main,
                    fileName=paste(filename,"index.html",sep=""),
                    pageTitle=title)
        toFile(page)
    }
    else {
        outfile <- file(filename,"w")
        cat(topText, file = outfile)
        if( table.center )
            cat("<CENTER> \n", file=outfile)

        cat("<TABLE BORDER=1>", file = outfile, sep = "\n")
        cat("<TR>",headOut,"</TR>", file=outfile, sep="\n")

        tds <- paste("<TD>",anchors,"</TD><TD>",dates,"</TD>",sep="")
        for (td in tds)
            cat("<TR>", td, "</TR>", file=outfile,sep="\n")

        cat("</TABLE>",file=outfile)
        if( table.center )
            cat("</CENTER> \n", file=outfile)
        cat("</body>", "</html>", sep = "\n", file = outfile)
        close(outfile)
    }
    invisible(NULL)
}

htmlpage <- function (genelist, filename, title, othernames, table.head,
                         table.center = TRUE, repository = "ll"){
  outfile <- file(filename, "w")
  type <- "text/css"
  cat("<html>", "<head>", "<TITLE>Differentially Expressed Genes</TITLE>",
      "</head>", "<body bgcolor=#FFFFFF >", "<H1 ALIGN=CENTER > Differentially Expressed Genes </H1>",
      # CSS to allow reasonable spacing for multiple links per cell
      paste("<style type=", type,">",sep = ""), "p{ margin-top: 1px; margin-bottom: 1px; padding-left: 10px; text-indent: -10px }",
      "</style>", file = outfile, sep = "\n")
  if (!missing(title))
    cat("<CENTER><H1 ALIGN=\"CENTER\">", title, " </H1></CENTER>\n",
        file = outfile, sep = "\n")
  if (table.center)
    cat("<CENTER> \n", file = outfile)
  cat("<TABLE BORDER=4>", file = outfile, sep = "\n")
  if (!missing(table.head)) {
    headout <- paste("<TH>", table.head, "</TH>")
    cat("<TR>", headout, "</TR>", file = outfile, sep = "\n")
  }
  if(is.list(genelist)){
    if(all.equal(rep(length(genelist[[1]]), length(genelist)),
                 unlist(lapply(genelist, length), use.names=FALSE))){
      nrows <- length(genelist[[1]])
    }else stop("The lists of genes to annotate must all be of equal length.")
  }else nrows <- length(genelist)

  if (is.list(repository)){
    rows <- ""
    for(i in seq(along=repository)){
      rows <- paste(rows, getTDRows(genelist[[i]], repository[[i]]))
    }
  }
  else rows <- getTDRows(genelist, repository)
  if (!missing(othernames)) {
    if (is.list(othernames)) {
      others <- ""
      for (nm in othernames){
        if(is.matrix(nm)){
          for(i in 1:dim(nm)[2]){
            others <- paste(others, "<TD>", nm[,i], "</TD>", sep = "")
          }
        }
        if(is.list(nm)){
          ## The assumption here is that if nm is a list, for some cells there will be
          ## multiple lines. This code just makes those cells multi-line.
          out <- vector()
          for(j in seq(along = nm)){
            out[j] <- paste("<P>", nm[[j]], "</P>", sep="", collapse="")
          }
          out <- paste("<TD>", out, "</TD>", sep="")
          others <- paste(others, out, sep="")
        }
        if(is.vector(nm) && !is.list(nm))
          others <- paste(others, "<TD>", nm, "</TD>", sep = "")
      }
    }
    else others <- paste("<TD>", othernames, "</TD>", sep = "")
    rows <- paste(rows, others)
  }
  for (i in 1:nrows) cat("<TR>", rows[i], "</TR>", file = outfile,
                         sep = "\n")
  cat("</TABLE>", file = outfile)
  if (table.center)
    cat("</CENTER> \n", file = outfile)
  cat("</body>", "</html>", sep = "\n", file = outfile)
  close(outfile)
}

getCells <-  function(ids, repository = "ug"){
  # This function allows us to insert multiple links in each cell by
  # building up the HTML more incrementally. Passing a list of character
  # vectors will result in multiple links per cell. Otherwise we get one link per cell.
  
  if(is.list(ids)){
    out <- vector()
    temp <- lapply(ids, getQueryLink, repository=repository)
    for(i in seq(along = ids)){
      if(temp[i] != "&nbsp;")
        out[i] <- paste("<P><A HREF=\"", temp[[i]], "\">",
                        ids[[i]], "</A></P>", sep = "", collapse="")
      else
        out[i] <- temp[i]
    }
  }else{
    temp <- getQueryLink(ids, repository)
    blanks <- temp == "&nbsp;"
    out <- paste(" <A HREF=\"", temp, "\">",
                 ids, "</A>", sep = "")
    out[blanks] <- "&nbsp;"
  }
  return(out)
}

getQueryLink <-function (ids, repository = "ug"){
  switch(tolower(repository), ug = return(getQuery4UG(ids)),
         ll = return(getQuery4LL(ids)), affy = return(getQuery4Affy(ids)),
         gb = return(getQuery4GB(ids)), sp = return(getQuery4SP(ids)),
         omim = return(getQuery4OMIM(ids)), fb = return(getQuery4FB(ids)),
         en = return(getQuery4EN(ids)), stop("Unknown repository name"))
}


getTDRows <- function (ids, repository = "ug"){
  # Modification of Jianhua's original code to allow for multiple links per cell.
  out <- paste("<TD>", getCells(ids, repository), "</TD>", sep="")
  return(out)
}

getQuery4Affy <- function (ids){
  # Affy IDs are all over the map, so there is no good way to catch any garbage input.
  # Here we have to rely on the end user to filter out garbage by passing an empty cell.
  blanks <- ids == "&nbsp;"
  out <- paste("https://www.affymetrix.com/LinkServlet?&probeset=",
               ids, sep="")
  out[blanks] <- "&nbsp;"
  return(out)
}

getQuery4UG <- function (ids){
  # Slight modification of Jianhua's original code, replacing error message with
  # empty cells in the table.
  if(is.factor(ids))
    ugs <- strsplit(as.character(ids), "\\.")
  else
    ugs <- strsplit(ids, "\\.")
  badUG <- function(x) if (length(x) != 2 || nchar(x[1]) !=
                           2)
    return(TRUE)
  else return(FALSE)
  bIDs <- sapply(ugs, badUG)
  temp <- vector()
  for( i in seq(along=ids)){
    if(!bIDs[i])
    temp[i] <- paste("http://www.ncbi.nlm.nih.gov/UniGene/clust.cgi?ORG=",
                     ugs[[i]][1], "&CID=", ugs[[i]][2], sep = "")
    else
    temp[i] <- "&nbsp;"
  }
  return(temp)
}

getQuery4LL <- function (ids){
  ## Here we rely on Locus Link ids being all numeric to filter out garbage
  ## that will result in busted links.
  if(is.factor(ids)){
    options(warn = -1)
    ids <- as.numeric(as.character(ids))
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.character(ids)){
    options(warn = -1)
    ids <- as.numeric(ids)
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.numeric(ids))
    blanks <- is.na(ids)
  out <- paste("http://www.ncbi.nlm.nih.gov/LocusLink/LocRpt.cgi?l=",
               ids, sep = "")
  out[blanks] <- "&nbsp;"
  return(out)
}

getQuery4EN <- function (ids){
  ## Here we rely on Locus Link ids being all numeric to filter out garbage
  ## that will result in busted links.
  if(is.factor(ids)){
    options(warn = -1)
    ids <- as.numeric(as.character(ids))
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.character(ids)){
    options(warn = -1)
    ids <- as.numeric(ids)
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.numeric(ids))
    blanks <- is.na(ids)
  out <- paste("http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=Graphics&list_uids=",
               ids, sep = "")
  out[blanks] <- "&nbsp;"
  return(out)
}


getQuery4GB <- function (ids){
  # GenBank ids can be either GB or RefSeq, so there is no good way to filter garbage.
  # Again we rely on end user to pass blanks.
  blanks <- ids == "&nbsp;"
  out <- paste("http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=search&term=",
               ids, sep="")
  out[blanks] <- "&nbsp;"
  return(out)
}



getQuery4SP <- function(ids){
  ## SwissProt ids are not consistent enough to do any sort of garbage checking
  ## so here we rely on a blank being passed by the end user.
  blanks <- ids == "&nbsp;"
  out <- paste("http://us.expasy.org/cgi-bin/get-entries?disp=1&AC=",ids, sep="")
  out[blanks] <- "&nbsp;"
  return(out)
}

getQuery4OMIM <- function(ids){
  # Conversion here relies on the assumption that OMIM ids are all numeric
  # so any non-numeric entry must be some sort of garbage that will result in
  # a broken link.
  if(is.factor(ids)){
    options(warn = -1)
    ids <- as.numeric(as.character(ids))
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.character(ids)){
    options(warn = -1)
    ids <- as.numeric(ids)
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.numeric(ids))
    blanks <- is.na(ids)
  
  out <- paste("http://www.ncbi.nlm.nih.gov/entrez/dispomim.cgi?id=", ids)
  if(!is.null(blanks))
    out[blanks] <- "&nbsp;"

  return(out)
  
}

getQuery4FB <- function (ids){
  ## Function to build links to flybase for drosophila arrays
  ## Here I rely on the flybase number starting with FBgn
  ## The end user can also pass an empty cell identifier
  if(is.factor(ids))
    fbs <- strsplit(as.character(ids), "FBgn")
  else
    fbs <- strsplit(ids, "FBgn")
  badFB <- function(x) if(length(x) != 2 || nchar(x[1]) != 0)
    return(TRUE) else return(FALSE)
  bIDS <- sapply(fbs, badFB)
  out <- paste("http://flybase.bio.indiana.edu/.bin/fbidq.html?", 
                    ids, sep = "")
  out[bIDS] <- "&nbsp;"
  return(out)
}


#ll.htmlpage <- function (genelist, filename, title, othernames,
#                         table.head, table.center=TRUE,
#                         repository = "ll")
#{
#    .Deprecated("htmlpage", package="annotate")
#    outfile <- file(filename, "w")
#    cat("<html>", "<head>", "<TITLE>BioConductor Linkage List</TITLE>",
#        "</head>", "<body bgcolor=#708090 >",
#        "<H1 ALIGN=CENTER > BioConductor Linkage List </H1>",
#        file = outfile, sep = "\n")
#    if( !missing(title) )
#        cat("<CENTER><H1 ALIGN=\"CENTER\">", title, " </H1></CENTER>\n",
#            file=outfile, sep = "\n")

#    if( table.center )
#        cat("<CENTER> \n", file=outfile)

#    cat("<TABLE BORDER=4>", file = outfile, sep = "\n")
#    if( !missing(table.head) ) {
#        headout <- paste("<TH>", table.head, "</TH>")
#        cat("<TR>", headout, "</TR>", file=outfile, sep="\n")
#    }
#    rh <- "<TD> <A HREF=\"http://www.ncbi.nlm.nih.gov/LocusLink/LocRpt.cgi?l="
#    nrows <- length(genelist)
#    rows <- getTDRows(genelist, repository)
#    rows <- paste(rh, genelist, "\">", genelist, "</A> </TD>",
#        sep = "")
#    if( !missing(othernames) ) {
#        if( is.list(othernames) ) {
#            others <- ""
#            for(nm in othernames)
#                others <- paste(others,"<TD>", nm, "</TD>", sep="")
#        }
#        else
#            others <- paste("<TD>", othernames, "</TD>", sep="")
#        rows <- paste(rows, others)
#    }
#    for (i in 1:nrows)
#        cat("<TR>", rows[i], "</TR>", file = outfile, sep = "\n")
#    cat("</TABLE>",file=outfile)
#    if( table.center )
#        cat("</CENTER> \n", file=outfile)
#    cat("</body>", "</html>", sep = "\n", file = outfile)

#    close(outfile)
#}
