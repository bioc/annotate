getSeq4Acc <- function(accNum){
    #options(show.error.messages = FALSE)
    return(getSEQ(getGI(accNum)))
    #options(show.error.messages = TRUE)
}

getGI <- function(accNum){
    # Get the gi based on the Accession number
    gi <- readLines(paste("http://www.ncbi.nlm.nih.gov/entrez/",
                          "query.fcgi?db=Nucleotide&cmd=search&term=",
                          accNum, sep = ""))
    gi <- gsub(paste(".*gi\\|([0-9]+)\\|[a-zA-Z0-9]+\\|", accNum,
                     ".*", sep = ""), "\\1",
               gi[grep(paste("gi\\|.*\\|[a-zA-Z0-9]+\\|", accNum, ".*",
                                    sep = ""), gi)])
    if(length(gi) == 0){
        stop(paste("Can't obtain a gi number for", accNum))
    }else{
        return(gi)
    }
}

getSEQ <- function(gi){

    seq <- readLines(paste("http://www.ncbi.nlm.nih.gov/entrez/batchseq.cgi?",
                 "cmd=&txt=on&save=&cfm=&list_uids=", gi, "&",
                 "db=nucleotide&extrafeat=16&term=&view=fasta&",
                 "dispmax=20&SendTo=t&__from=&__to=&__strand=", sep = ""))

    if(length(seq) == 0){
        stop("Failed to extract the sequence")
    }else{
        return(paste(seq[2:length(seq)], sep = "", collapse = ""))
    }
}
