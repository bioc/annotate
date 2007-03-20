annPkgName <- function(name, type=c("db", "env")) {
    type <- match.arg(type)
    if (length(grep("db$", name)))
      if (type == "db")
        name
      else
        substr(name, 1, nchar(name)-2)
    else if (type == "db")
      paste(name, "db", sep="")
    else
      name
}

getAnnMap <- function(map, chip, load=FALSE, type=c("db", "env")) {
    badTypes <- type[!(type %in% c("db", "env"))]
    if (length(badTypes))
      stop("unknown types in 'type' argument: ",
           paste(badTypes, collapse=", "))
    pkg <- annPkgName(name=chip, type=type[1])
    searchName <- paste("package", pkg, sep=":")
    pkgEnv <- tryCatch(as.environment(searchName), error=function(e) {
        if (load) {
            ok <-
              suppressWarnings(require(pkg, character.only=TRUE,
                                       quietly=TRUE))
            if (!ok && length(type) > 1) {
                origPkg <- pkg
                for (t in type[2:length(type)]) {
                    pkg <- annPkgName(name=chip, type=t)
                    searchName <- paste("package", pkg, sep=":")
                    if (suppressWarnings(require(pkg, character.only=TRUE,
                                                 quietly=TRUE))) {
                        warning("getAnnMap: ", "package ", origPkg,
                                " not available, ", "using ", pkg, " instead",
                                call.=FALSE)
                        ok <- TRUE
                        break
                    }
                }
            }
            if (!ok)
              stop("getAnnMap: ", "package ", pkg, " not available",
                   call.=FALSE)
            as.environment(searchName)
        } else {
            stop("getAnnMap: ", pkg, " package not attached and load is FALSE",
                 call.=FALSE)
        }
    })
    mapName <- paste(chip, map, sep="")
    get(mapName, envir=pkgEnv)
}
