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

.onLoad <- function(libname, pkgname) {
    .buildAnnotateOpts()
    if(.Platform$OS.type == "windows" && require(Biobase) && interactive()
        && .Platform$GUI ==  "Rgui"){
        addVigs2WinMenu("annotate")
    }
}
