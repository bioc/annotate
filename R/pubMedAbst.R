.initPubMedAbst <- function(where) {

    ## Define the class structure of the pubMedAbst object
    setGeneric("pubMedAbst", function(object)
               standardGeneric("pubMedAbst"), where=where)

    setClass("pubMedAbst",
             representation(authors="vector", abstText="character",
             articleTitle="character", journal="character",
             pubDate="character", url="character"), where=where)

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
                   standardGeneric("pubData"), where=where)

    if (is.null(getGeneric("url")))
        setGeneric("url",function(object)
                   standardGeneric("url"),where=where)

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
    setMethod("url", "pubMedAbst", function(object)
              object@url, where=where)
}

buildPubMedAbst <- function(xml) {
    ## Passed in a XML tree detailing a single article
    ## will parse the XML and create a new class

    xmlArticle <- xml["MedlineCitation"][[1]]["Article"]

    articleTitle <- xmlArticle[[1]]["ArticleTitle"]
    articleTitle <- as.character(xmlChildren(articleTitle[[1]])$text)[5]

    abstText <- xmlArticle[[1]]["Abstract"][[1]]["AbstractText"]
    abstText <- as.character(xmlChildren(abstText[[1]])$text)[5]

    xmlJournal <- xmlArticle[[1]]["Journal"]
    pubDateBase <- xmlJournal[[1]]["JournalIssue"][[1]]["PubDate"]
    pubDateMonth <- pubDateBase[[1]]["Month"]
    pubDateMonth <- as.character(xmlChildren(pubDateMonth[[1]])$text)[5]

    journal <-
        xml["MedlineCitation"][[1]]["MedlineJournalInfo"][[1]]["MedlineTA"]

    journal <- as.character(xmlChildren(journal[[1]])$text)[5]

    pubDateYear <- pubDateBase[[1]]["Year"]
    pubDateYear <- as.character(xmlChildren(pubDateYear[[1]])$text)[5]
    pubDate <- paste(pubDateMonth,pubDateYear)

    authorList <- xmlArticle[[1]]["AuthorList"]
    authors <- vector()
    for (i in 1:length(xmlChildren(authorList[[1]]))) {
        curAuthor <- authorList[[1]][i]
        last <-
            as.character(xmlChildren(curAuthor[[1]]["LastName"][[1]])$text)[5]
        first <-
            as.character(xmlChildren(curAuthor[[1]]["ForeName"][[1]])$text)[5]
        mid <-
            as.character(xmlChildren(curAuthor[[1]]["Initials"][[1]])$text)[5]

        authors[i] <- paste(first,mid,last)
    }

    url <-
        as.character(xmlChildren(xml["PubmedData"][[1]]["URL"][[1]])$text)[5]


    newPMA <- new("pubMedAbst", articleTitle=articleTitle,
                  abstText=abstText, pubDate=pubDate,authors=authors,
                  journal=journal,url=url)
    return(newPMA)
}


