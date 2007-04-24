annPkgName <- function(name, type=c("env", "db")) {
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

getAnnMap <- function(map, chip, load=TRUE, type=c("env", "db")) {
    typeMissed <- FALSE
    searchName <- NULL
    if (missing(type)) {
        typeMissed <- TRUE
        searchNames <- paste("package:", chip, c("", "db"), sep="")
        searchPth <- search()
        whLoaded <- match(searchNames, searchPth)
        whLoaded <- whLoaded[!is.na(whLoaded)]
        if (length(whLoaded))
          searchName <- searchPth[sort(whLoaded)][1]
    } else {
        badTypes <- type[!(type %in% c("db", "env"))]
        if (length(badTypes))
          stop("unknown types in 'type' argument: ",
               paste(badTypes, collapse=", "))
    }
    pkg <- annPkgName(name=chip, type=type[1])
    if (is.null(searchName))
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
                        if (!typeMissed)
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
