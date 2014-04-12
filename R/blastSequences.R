## Try five times and give error if all attempts fail.
.tryParseResult <- function(url, rtoe, timeout) {
    start <- Sys.time()
    end <- Sys.time() + timeout
    repeat {
        Sys.sleep(min(rtoe, end - Sys.time()))
        result <- tryCatch({
            xmlParse(url, error = xmlErrorCumulator(immediate=FALSE))
        }, XMLParserErrorList=function(err) {
            NULL
        })
        if (!is.null(result))
            return(result)
        if (Sys.time() > end)
            break
    }
    stop("'blastSequences' timeout after ", as.integer(Sys.time() - start),
         " seconds", call.=FALSE)
}

## Using the REST-ish API described at
## http://www.ncbi.nlm.nih.gov/blast/Doc/node2.html
blastSequences <- function(x,database="nr",
                           hitListSize="10",
                           filter="L",
                           expect="10",
                           program="blastn",
                           timeout=40)
{
   require(XML)

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
   message("estimated response time: ", rtoe, " seconds")
   result <- .tryParseResult(url1, rtoe, timeout)
   qseq <- xpathApply(result, "//Hsp_qseq", xmlValue)
   hseq <- xpathApply(result, "//Hsp_hseq", xmlValue)

   ## Instead lets put it into a DNAStringSet and make a MultipleSeqAlignment
   ## out of it.
   require(Biostrings)
   res <- vector("list", length(qseq))
   for(i in seq_along(qseq)){
     res[[i]] <- Biostrings::DNAMultipleAlignment(
         c(hseq[[i]],qseq[[i]]), rowmask=as(IRanges(), "NormalIRanges"),
         colmask=as(IRanges(), "NormalIRanges"))
   }
   res 
}

## took 11.5 minutes to do a blast...  (ugh)
