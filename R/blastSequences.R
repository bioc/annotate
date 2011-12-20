## Try three times and give error if all attempts fail.
.tryParseResult <- function(url){
  tf <- tempfile()
  
  for(i in 1:4)
  {
      download.file(url, tf)
      f = file(tf)
      lines <- readLines(tf)
      close(f)
      statusLine <- grep("Status=", lines, value=TRUE)
      if (length(statusLine) == 0) ## is this XML?
          return(xmlTreeParse(tf, useInternalNodes=TRUE))
      if (i < 4)
        Sys.sleep(10)
  }
  msg = paste("After 3 attempts, annotate is still not getting", 
              "results from the web service. Please try again later.", 
               sep=" ")
  stop(paste(strwrap(msg,exdent=2), collapse="\n"))  
}


## Using the REST-ish API described at
## http://www.ncbi.nlm.nih.gov/blast/Doc/node2.html
blastSequences <- function(x,database="nr",
                           hitListSize="10",
                           filter="L",
                           expect="10",
                           program="blastn"){
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
   ## wait 5 seconds before request, to be polite, as requested
   ## by the documentation (if we are going to make several requests).
   Sys.sleep(5)
   resCode <- download.file(url0, results)
   f = file(results)
   lines <- readLines(f)
   close(f)
   ## Look for job ID
   ridLine <- grep("RID = ([.]*)", lines, value=TRUE)
   m <- regexec("RID = ([^\n]*)", ridLine)
   rm <- regmatches(ridLine, m)
   rid <- rm[[1]][2]
   ## Look for RTOE (estimated time to completion)
   rtoeLine <- grep("RTOE = ",  lines, value=TRUE)
   m <- regexec("RTOE = ([^\n]*)", rtoeLine)
   rm <- regmatches(rtoeLine, m)
   rtoe <- as.integer(rm[[1]][2])
   url1 <- sprintf("%s?RID=%s&FORMAT_TYPE=XML&CMD=Get", baseUrl, rid)
   ## wait RTOE seconds
   Sys.sleep(rtoe)
   require(XML)
   result <- .tryParseResult(url1)
   qseq <- xpathApply(result, "//Hsp_qseq", xmlValue)
   hseq <- xpathApply(result, "//Hsp_hseq", xmlValue)

   ## Instead lets put it into a DNAStringSet and make a MultipleSeqAlignment
   ## out of it.
   require(Biostrings)
   res <- list()
   for(i in seq_len(length(qseq))){
     res[i] <- DNAMultipleAlignment( c(hseq[[i]],qseq[[i]]),
                                     rowmask=as(IRanges(), "NormalIRanges"),
                                     colmask=as(IRanges(), "NormalIRanges"))
   }
   res 
}

