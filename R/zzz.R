.buildAnnotateOpts <- function() {
    if (is.null(getOption("BioC"))) {
        BioC <- list()
        class(BioC) <- "BioCOptions"
        options("BioC"=BioC)
    }

    Annotate <- list()
    class(Annotate) <- "BioCPkg"
    Annotate$urls <- list()
    Annotate$urls$ncbi <- "http://www.ncbi.nih.gov/"

    BioC <- getOption("BioC")
    BioC$Annotate <- Annotate
    options("BioC"=BioC)
}

.First.lib <- function(libname, pkgname, where) {
    require(Biobase)

  if(missing(where)) {
        where <- match(paste("package:", pkgname, sep=""), search())
        if(is.na(where)) {
            warning(paste("Not a package name: ",pkgname))
            return()
        }
        where <- pos.to.env(where)
    }

    .initChromLoc(where)
    .initChromLocation(where)

    .buildAnnotateOpts()
}
