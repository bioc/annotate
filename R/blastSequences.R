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
  ## Service needs TIME to generate this file.  So do some counting.
  Sys.sleep(20) ## 10 seconds sometimes works, but sometimes not.
  result <- xmlTreeParse(url1, useInternalNodes=TRUE)  
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




