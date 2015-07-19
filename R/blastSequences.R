.blastSequencesToDNAMultipleAlignment <- function(xml) {
   qseq <- xpathSApply(xml, "//Hsp_qseq", xmlValue)
   hseq <- xpathSApply(xml, "//Hsp_hseq", xmlValue)
   require(Biostrings)
   res <- vector("list", length(qseq))
   for(i in seq_along(qseq)){
     res[[i]] <- Biostrings::DNAMultipleAlignment(
         c(hseq[[i]],qseq[[i]]), rowmask=as(IRanges(), "NormalIRanges"),
         colmask=as(IRanges(), "NormalIRanges"))
   }
   res
}

.blastSequencesToDataFrame <- function(xml) {
    if (xpathSApply(xml, "count(//Hit)") == 0L) {
        message("'blastSequences' returned 0 matches")
        return(data.frame())
    }

    iter <- xml["//Iteration"]
    iterlen <- sapply(iter, xpathSApply, "count(.//Hsp)")
    iterdf <- xmlToDataFrame(iter, stringsAsFactors=FALSE)

    hit <- xml["//Hit"]
    hitlen <- sapply(hit, xpathSApply, "count(.//Hsp)")
    hitdf <- xmlToDataFrame(hit, stringsAsFactors=FALSE)
    hitdf <- hitdf[, names(hitdf) != "Hit_hsps", drop=FALSE]

    hsp <- xmlToDataFrame(xml["//Hsp"] , stringsAsFactors=FALSE)

    df <- cbind(
        iterdf[rep(seq_len(nrow(iterdf)), iterlen),, drop=FALSE],
        hitdf[rep(seq_len(nrow(hitdf)), hitlen),, drop=FALSE],
        hsp)
    rownames(df) <- NULL
    df
}

.tryParseResult <- function(url, rtoe, timeout) {
    message("estimated response time ", rtoe, " seconds")
    start <- Sys.time()
    end <- Sys.time() + timeout
    repeat {
        dt <- max(1, as.double(end - Sys.time(), units="secs"))
        Sys.sleep(min(rtoe, dt))
        result <- tryCatch({
            xmlParse(url, error = xmlErrorCumulator(immediate=FALSE))
        }, XMLParserErrorList=function(err) {
            NULL
        })
        if (!is.null(result))
            return(result)
        elapsed <- as.double(Sys.time() - start, units="secs")
        if (Sys.time() > end) {
            if (interactive()) {
                msg <- sprintf("timeout after %.0f seconds; wait another %d seconds? [y/n] ",
                               elapsed, timeout)
                repeat {
                    ans <- substr(tolower(readline(msg)), 1, 1)
                    if (ans %in% c("y", "n"))
                        break
                }
                if (ans == "y") {
                    end <- Sys.time() + timeout
                    next
                }
            }
            break
        } else {
            message(sprintf("elapsed time %.0f seconds", elapsed))
        }
    }
    msg <- sprintf("'blastSequences' timeout after %.0f seconds",
                   elapsed)
    stop(msg, call.=FALSE)
}

## Using the REST-ish API described at
## http://www.ncbi.nlm.nih.gov/blast/Doc/node2.html
blastSequences <- function(x,database="nr",
                           hitListSize="10",
                           filter="L",
                           expect="10",
                           program="blastn",
                           timeout=40,
                           as=c("DNAMultipleAlignment", "data.frame", "XML"))
{
    PARSE <- switch(match.arg(as),
                    DNAMultipleAlignment=.blastSequencesToDNAMultipleAlignment,
                    data.frame=.blastSequencesToDataFrame,
                    XML=identity)
    ## TODO: lots of argument checking and testing.  Also,
    ## depending on which program string is used we need to make the correct
    ## kind of object at the end (so blastn means DNAMultipleAlignment, and
    ## blastp means AAMultipleAlignment etc.

    ## So:
    ## 1) get online values these parameters can be
    ## 2) document those
    ## 3) restrict their vals in the code here.
    ## 4) for program, use this to determine what object is returned.
    
    ## assemble the query
    baseUrl <- "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi"
    query <- paste("QUERY=",as.character(x),"&DATABASE=",database,
                   "&HITLIST_SIZE=",hitListSize,"&FILTER=",filter,
                   "&EXPECT=",expect,"&PROGRAM=",program, sep="")
    url0 <- sprintf("%s?%s&CMD=Put", baseUrl, query)
    results <- tempfile()
    post <- htmlParse(url0)
    
    x <- post[['string(//comment()[contains(., "QBlastInfoBegin")])']]
    rid <- sub(".*RID = ([[:alnum:]]+).*", "\\1", x)
    rtoe <- as.integer(sub(".*RTOE = ([[:digit:]]+).*", "\\1", x))
    url1 <- sprintf("%s?RID=%s&FORMAT_TYPE=XML&CMD=Get", baseUrl, rid)
    result <- .tryParseResult(url1, rtoe, timeout)
    PARSE(result)
}

## took 11.5 minutes to do a blast...  (ugh)
