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

.First.lib <- function(libname, pkgname) {
    require(Biobase) || stop("cannot load annotate without Biobase")
    .buildAnnotateOpts()
    if(.Platform$OS.type == "windows" && require(Biobase) && interactive()
        && .Platform$GUI ==  "Rgui"){
        addPDF2Vig("annotate")
    }
}
