
# Query the GEO database. url is the common CGI scrip at GEO
# and GEOAccNum is the GEO accession number representing a file in the
# database
readIDNAcc <- function(GEOAccNum, url =
                       "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?"){
    temp <- readGEOAnn(GEOAccNum, url)
    return(temp[,c("ID", "GB_ACC")])
}


# Query the GEO database. url is the common CGI scrip at GEO
# and GEOAccNum is the GEO accession number representing a file in the
# database
readGEOAnn <- function(GEOAccNum, url =
                       "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?"){

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

# Read from GEO and map GEO accession numbers to array names.
getGPLNames <- function(url =
                        "http://www.ncbi.nlm.nih.gov/geo/query/browse.cgi?"){
    conn <- url(paste(url,
                      "view=platforms&prtype=nucleotide&dtype=commercial",
                      sep = ""))
    temp <- readLines(conn)
    close(conn)

    temp <- temp[grep("<TD", temp)]
    temp <- matrix(temp, ncol = 8, byrow = TRUE)
    #temp <- temp[, c(1, 6)]
    #temp[,1] <- gsub(".*>(.*)</a>$", "\\1", temp[,1])
    #temp[,2] <- gsub(".*>(.*)</TD>$", "\\1", temp[,2])

    chipNames <- gsub(".*>(.*)</TD>$", "\\1", temp[,6])
    names(chipNames) <- gsub(".*>(.*)</a>$", "\\1", temp[,1])

    return(chipNames)
}
