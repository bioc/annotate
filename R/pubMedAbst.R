.initPubMedAbst <- function(where) {

    ## Define the class structure of the pubMedAbst object
    setGeneric("pubMedAbst", function(object)
               standardGeneric("pubMedAbst"), where=where)

    setClass("pubMedAbst",
             representation(authors="vector", abstText="character",
             articleTitle="character", journal="character",
             pubDate="character", abstUrl="character"), where=where)

    ## Define accessors
    if (is.null(getGeneric("authors")))
        setGeneric("authors", function(object)
                   standardGeneric("authors"), where=where)

    if (is.null(getGeneric("abstText")))
        setGeneric("abstText", function(object)
                   standardGeneric("abstText"), where=where)

    if (is.null(getGeneric("articleTitle")))
        setGeneric("articleTitle", function(object)
                   standardGeneric("articleTitle"), where=where)

    if (is.null(getGeneric("journal")))
        setGeneric("journal", function(object)
                   standardGeneric("journal"), where=where)

    if (is.null(getGeneric("pubDate")))
        setGeneric("pubDate", function(object)
                   standardGeneric("pubDate"), where=where)

    if (is.null(getGeneric("abstUrl")))
        setGeneric("abstUrl",function(object)
                   standardGeneric("abstUrl"),where=where)

    ## Methods
    setMethod("authors", "pubMedAbst", function(object)
              object@authors, where=where)
    setMethod("abstText", "pubMedAbst", function(object)
              object@abstText, where=where)
    setMethod("articleTitle", "pubMedAbst", function(object)
              object@articleTitle, where=where)
    setMethod("journal", "pubMedAbst", function(object)
              object@journal, where=where)
    setMethod("pubDate", "pubMedAbst", function(object)
              object@pubData, where=where)
    setMethod("abstUrl", "pubMedAbst", function(object)
              object@abstUrl, where=where)
}

buildPubMedAbst <- function(xml) {
    ## Passed in a XML tree detailing a single article
    ## will parse the XML and create a new class

    xmlArticle <- xml["MedlineCitation"][[1]]["Article"]

    ## Disable error messages, and wrap potential error causers
    ## w/ trys
    options(show.error.messages = FALSE)
    on.exit(options(show.error.messages=TRUE))

    ## Retrieve Article Title
    articleTitle <- xmlArticle[[1]]["ArticleTitle"]
    articleTitle <-
    try(as.character(xmlChildren(articleTitle[[1]])$text)[5])
    if (inherits(articleTitle,"try-error") == TRUE) {
        articleTitle <- "No Title Provided"
    }

    ## Retrieve the abstract
    abstText <- xmlArticle[[1]]["Abstract"][[1]]["AbstractText"]
    abstText <- try(as.character(xmlChildren(abstText[[1]])$text)[5])
   if (inherits(abstText,"try-error") == TRUE) {
       abstText <- "No Abstract Provided"
   }

    ## Retrieve the date - get the year/month separately and then
    ## join them at the end.  If no month or year provided, subst
    ## "MontH" and "Year" respectively
    pubDateBase <-
        xmlArticle[[1]]["Journal"][[1]]["JournalIssue"][[1]]["PubDate"]
    pubDateMonth <- pubDateBase[[1]]["Month"]
    pubDateMonth <-
        try(as.character(xmlChildren(pubDateMonth[[1]])$text)[5])
    if (inherits(pubDateMonth,"try-error") == TRUE) {
        pubDateMonth <- "Month"
    }
    pubDateYear <- pubDateBase[[1]]["Year"]
    pubDateYear <-
        try(as.character(xmlChildren(pubDateYear[[1]])$text)[5])
    if (inherits(pubDateYear, "try-error") == TRUE) {
        pubDateYear <- "Year"
    }
    ## Join up the date information
    pubDate <- paste(pubDateMonth,pubDateYear)

    ## Get the journal this was published in
    journal <-
        xml["MedlineCitation"][[1]]["MedlineJournalInfo"][[1]]["MedlineTA"]
    journal <- try(as.character(xmlChildren(journal[[1]])$text)[5])
    if (inherits(journal,"try-error") == TRUE) {
        journal <- "No Journal Provided"
    }

    ## Build up a vector of author names, created by assembling the
    ## pieces of each author's name.
    authorList <- xmlArticle[[1]]["AuthorList"]
    authors <- vector()
    numAuthors <- try(length(xmlChildren(authorList[[1]])))
    if (inherits(numAuthors,"try-error") == TRUE) {
        authors[1] <- "No Author Information Provided"
    }
    else {
        for (i in 1:numAuthors) {
            curAuthor <- authorList[[1]][i]
            last <-
                try(as.character(xmlChildren(curAuthor[[1]]["LastName"][[1]])$text)[5])
            if (inherits(last,"try-error") == TRUE) {
                last <- "LastName"
            }

            first <-
                try(as.character(xmlChildren(curAuthor[[1]]["ForeName"][[1]])$text)[5])
            if (inherits(first,"try-error") == TRUE) {
                first <- "FirstName"
            }

            mid <-
                try(as.character(xmlChildren(curAuthor[[1]]["Initials"][[1]])$text)[5])
            if (inherits(mid,"try-error") == TRUE) {
                mid <- "M"
            }

            authors[i] <- paste(first,mid,last)
        }
    }

    abstUrl <-
        try(as.character(xmlChildren(xml["PubmedData"][[1]]["URL"][[1]])$text)[5])
    if (inherits(abstUrl,"try-error") == TRUE) {
        abstUrl <- "No URL Provided"
    }

    ## Restore error messages
    options(show.error.messages=TRUE)

    newPMA <- new("pubMedAbst", articleTitle=articleTitle,
                  abstText=abstText, pubDate=pubDate,authors=authors,
                  journal=journal,abstUrl=abstUrl)

    return(newPMA)
}

pm.getabst <- function(geneids, basename) {
    pmenvN <- paste(basename, "PMID", sep="")
    library(basename, character.only=TRUE)
    if( !exists(pmenvN, mode = "environment") ) 
        stop("could not access PubMed ids for this data")
    pmenv <- get(pmenvN)
    pmids <- multiget(geneids, env=pmenv)
    numids <- length(geneids)
    rval <- vector("list", length=numids)
    for(i in 1:numids) {
        pm <- pmids[[i]]
        if( is.na(pm) ) 
            rval[[i]] <- NA
        else {
            absts <- pubmed(pm)
            a <- xmlRoot(absts)
            numAbst <- length(xmlChildren(a))
            absts <- vector("list", length=numAbst)
            for (j in 1:numAbst)
                absts[[j]] <- buildPubMedAbst(a[[j]])
            rval[[i]] <- absts
        }
    }
    rval
}

pm.abstGrep <- function(pattern, absts, ...)
{
    nabsts <- length(absts)
    rval <- rep(FALSE, nabsts)
    for(i in 1:nabsts) {
        atxt <- abstText(absts[[i]])
        ans <- grep(pattern, atxt, ...)
        if( length(ans) && ans==1 )
            rval[i] <- TRUE
    }
    rval
}

pm.titles <- function (absts) {
     numa <- length(absts)
     rval <- vector("list", length=numa)
     for(j in 1:numa)
         rval[[j]] <- sapply(absts[[j]], function(x) articleTitle(x))
     rval
}

