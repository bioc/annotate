.buildAnnotateOpts <- function() {
    if (is.null(getOption("BioC"))) {
        BioC <- list()
        class(BioC) <- "BioCOptions"
        options("BioC"=BioC)
    }

    Annotate <- list()
    class(Annotate) <- "BioCPkg"
    Annotate$urls <- list( ncbi = "http://www.ncbi.nih.gov/",
          data="http://www.bioconductor.org/datafiles/annotate/")

    BioC <- getOption("BioC")
    BioC$annotate <- Annotate
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

    options(pdfViewer="acroread")

    .initChromLoc(where)
    .initChromLocation(where)
    .initPubMedAbst(where)

    .buildAnnotateOpts()
}
