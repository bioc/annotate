
# Query the GEO database. url is the common CGI scrip at GEO
# and GEOAccNum is the GEO accession number representing a file in the
# database
readIDNAcc <- function(url =
             "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?", GEOAccNum){
    temp <- readGEOAnn(url, GEOAccNum)
    return(temp[,c("ID", "GB_ACC")])
}


# Query the GEO database. url is the common CGI scrip at GEO
# and GEOAccNum is the GEO accession number representing a file in the
# database
readGEOAnn <- function(url =
             "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?", GEOAccNum){

    conn <- url(paste(url, "acc=", GEOAccNum,
                   "&view=data&form=text&targ=self", sep = ""), open = "r")
    temp <- readLines(conn)
    close(conn)
    # Remove the header lines that come with the file
    temp <- temp[grep("\t", temp)]
    # Add NAs to lines with no value for the last column
    temp <- strsplit(gsub("\t$", "\tNA", temp), "\t")
    # Convert to a matrix
    temp <- t(sapply(temp, unlist))
    # The first row is for column name. Remove it.
    colnames(temp) <- temp[1,]
    return(temp[-1,])
}
