## Try three times and give error if all attempts fail.
.tryParseResult <- function(url){
  for(i in 1:4){
     tryResult <- try(xmlTreeParse(url, useInternalNodes=TRUE), silent=TRUE)
     if(is(tryResult,"try-error") && i < 4){
         Sys.sleep(20)
     }else if(is(tryResult,"try-error") && i >= 4){
         msg = paste("After 3 attempts, annotate is still not getting", 
                     "results from the web service. Please try again later.", 
                      sep=" ")
         stop(paste(strwrap(msg,exdent=2), collapse="\n"))  
     }else{
	result <- xmlTreeParse(url, useInternalNodes=TRUE)
	return(result)
     }
  }
}


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
  
  require(XML)
  ## assemble the query
  baseUrl <- "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi"
  query <- paste("QUERY=",as.character(x),"&DATABASE=",database,
                 "&HITLIST_SIZE=",hitListSize,"&FILTER=",filter,
                 "&EXPECT=",expect,"&PROGRAM=",program, sep="")
  url0 <- sprintf("%s?%s&CMD=Put", baseUrl, query)

  ## Then submit the query and extract the return result id (RID) using XML
  ## and XPATH queries (see http://www.w3.org/TR/xpath/, especially section 2.5)
  post <- htmlTreeParse(url0, useInternalNodes=TRUE)
  rid <- xpathApply(post, '//input[@name="RID"][@id="rid"]',
                    xmlAttrs)[[1]][["value"]]

  ## and then finally retrieve and parse the result
  url1 <- sprintf("%s?RID=%s&FORMAT_TYPE=XML&CMD=Get", baseUrl, rid)
  ## Service needs TIME to generate this file.  So try a few times as needed.
  Sys.sleep(20) ## almost always seems to need this much sleep.  
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
