serializeEnv <- function(env, fname) {
 if (!is.character(fname))
     stop("conn should be a character name of file for storage")

 if (is.character(env)) {
     cmd <- paste("envList <- as.list(", env, ")")
     eval(parse(text=cmd))
 }
 else if (is.environment(env))
     envList <- as.list(env)
 else
     stop("invalid 'env' argument")

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

serializeDataPkgEnvs <- function(pkgDir) {
    pkg <- basename(pkgDir)
    require(pkg, character.only=TRUE) || stop("data package ",
                 pkg, " not installed")

    cDir <- getwd()
    on.exit(setwd(cDir), add=TRUE)
    setwd(pkgDir)

    if (! file.exists("inst"))
        if (!dir.create("inst"))
            stop("Failed to create inst for ", pkgDir)
    if (! file.exists(file.path("inst", "gdbm")))
        if (!dir.create(file.path("inst", "gdbm")))
            stop("Failed to create inst/gdbm for ", pkgDir)
    setwd("inst/gdbm")

    dataSets <- ls(paste("package", pkg, sep=":"))
    if (length(dataSets) == 0)
        return(0)
    dataSets <- dataSets[dataSets != pkg]

    for (i in seq(along=dataSets))
        serializeEnv(dataSets[i], paste(dataSets[i], ".xml.gz", sep=""))

    i
}
