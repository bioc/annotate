serializeEnv <- function(env, fname) {
 if (!is.character(fname))
     stop("conn should be a character name of file for storage")

 envList <- as.list(env)
 keys <- names(envList)

 out <- "<?xml version=\"1.0\"?>\n"
 out <- paste(out,
              "<values xmlns:bt=\"http://www.bioconductor.org/RGDBM\">")
 for (i in seq(along=envList)) {
     out <- paste(out, "\n\t<entry>\n\t\t<key>\n\t\t\t", keys[i],
                  "\n\t\t</key>\n\t\t<value>\n\t\t\t",
                  serialize(envList[[i]], NULL, ascii=TRUE),
                  "\n\t\t</value>\n\t</entry>", sep="")

 }
 out <- paste(out, "\n</values>", sep="")

 outFile <- gzfile(fname)
 open(outFile, open="wb")
 cat(out, file=outFile)
 close(outFile)
}

