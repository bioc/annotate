neighborGeneFinder <- function(geneData, keyName = c("unigene", "locuslink"),
                               organism = c("human",  "mouse", "rat")){

    .Deprecated("none", package="annotate", msg = "is no longer supported")
    require("tkWidgets", character.only = TRUE) ||
    stop(paste("neighbotGeneFinder requires the",
                             "tkWidgets package. Please have it installed"))
    organism <- tolower(match.arg(organism))
    keyName <- tolower(match.arg(keyName))
    collection <- list()
    neighbors <- NULL

    if(keyName == "unigene"){
        if(!require(paste(organism, "LLMappings", sep = ""),
                   character.only = TRUE)){
            stop(paste("Package ", organism, "LLMappings unavailable",
                      sep = ""))
        }
    }
    if(!require(paste(organism, "CHRLOC", sep = ""),
                character.only = TRUE)){
        stop(paste("Package ", organism, "CHRLOC unavailable",  sep = ""))
    }
    if(!is.element(keyName, colnames(geneData))){
        stop(paste("Key name", keyName, "is not one of the columns",
                   "of argument geneData"))
    }
    elementSelected <- function(index){
        for(i in 1:length(geneList)){
            if(i != index){
                tkselection.clear(geneList[[i]], 0, tksize(geneList[[i]]))
                tkselection.set(geneList[[i]],
                                tkcurselection(geneList[[index]]))
            }
        }
    }
    bindLists <- function(){
        for(i in 1:length(geneList)){
             selFun <- function(){}
             body <- list(as.name("{"),
                          substitute(elementSelected(j), list(j = i)),
                          substitute(findUDGenes()))
             body(selFun) <- as.call(body)
             tkbind(geneList[[i]], "<B1-ButtonRelease>", selFun)
         }
    }
    upDateNames <- function(){
        for(i in colnames(geneData)){
            writeList(geneList[[i]], geneData[,i], clear = TRUE)
        }
    }
    end <- function(){
        tkdestroy(base)
    }
    collect <- function(){
        key <- tclvalue(tkget(geneList[[keyName]],
                              (tkcurselection(geneList[[keyName]]))))
        if(!is.null(neighbors)){
            collection[[key]] <<- neighbors
        }
        tkconfigure(collectBut, state = "disabled")
    }
    findUDGenes <- function(){
        # Clean the list boxes
        tkdelete(upNeighbors, 0, "end")
        tkdelete(downNeighbors, 0, "end")
        key  <- tclvalue(tkget(geneList[[keyName]],
                              (tkcurselection(geneList[[keyName]]))))
        if(keyName == "unigene"){
            options(show.error.messages = FALSE)
            datafile = get(paste(organism, "LLMappingsUG2LL", sep = ""))
            tempKey <- try(datafile[[key]])
            options(show.error.messages = TRUE)
            if(inherits(tempKey, "try-error")){
                tkmessageBox(title = "No map  error",
                             message = paste("Can not map", key,
                             " to a gene"), icon = "warning",
                             type = "ok")
                return(invisible())
            }else{
                key <- tempKey
            }
        }
        neighbors <<- findNeighbors(paste(organism, "CHRLOC", sep = ""),
                                   key, upBase = tclvalue(upBase),
                                   downBase = tclvalue(downBase),
                                   mergeOrNot = FALSE)
        upstream <- NULL
        downstream <- NULL
        for(i in names(neighbors)){
            upstream <- c(upstream, c(paste("chromosome", i),
                              as.vector(neighbors[[i]]$upstream), "  "))
            downstream <- c(downstream, c(paste("chromosome", i),
                              as.vector(neighbors[[i]]$downstream), "  "))
        }
        writeList(upNeighbors, upstream)
        writeList(downNeighbors, downstream)

        tkconfigure(collectBut, state = "normal")
        tkconfigure(locatBut, state = "normal")
    }

    base <- tktoplevel()
    on.exit(tkdestroy(base))
    tktitle(base) <- "BioC Neighbor Genes Finder"

    conFrame <- tkframe(base)

    leftFrame <- tkframe(conFrame)
    tkpack(tklabel(leftFrame, text = "Gene names"),
           expand = FALSE, fill = "x", pady = 5)
    geneFrame <- tkframe(leftFrame)
    geneList <- oneVScrList(geneFrame, geneData)
    tkconfigure(geneList[[keyName]], exportselection = FALSE)
#    tkbind(geneList[[keyName]], "<B1-ButtonRelease>", findUDGenes)
    bindLists()
    tkpack(geneFrame, expand = TRUE, fill = "y")

    tkpack(leftFrame, padx = 4, side = "left", expand = FALSE, fill = "y")

    rightFrame <- tkframe(conFrame)
    tkpack(tklabel(rightFrame, text = "Neighboring genes within"),
           side = "top", fill = "x", pady = 5)
    neighborFrame <- tkframe(rightFrame)
    upFrame <- tkframe(neighborFrame)
    baseFrame <- tkframe(upFrame)
    upBase <- tclVar(50000)
    tkpack(tkentry(baseFrame, textvariable = upBase, width = 10),
           expand = TRUE, fill = "x", side = "left")
    tkpack(tklabel(baseFrame, text = "bases up"), side = "left",
           expand = FALSE, fill = "x")
    tkpack(baseFrame, expand = FALSE, fill = "x")
    UNFrame <- tkframe(upFrame)
    upNeighbors <- makeViewer(UNFrame, vHeight = 10, vWidth = 15,
                          vScrol = TRUE, hScrol = FALSE, what = "list")
    tkpack(UNFrame, expand = TRUE, fill = "both")
    tkpack(upFrame, side = "left", expand = TRUE, fill = "y", padx = 5)

    downFrame <- tkframe(neighborFrame)
    baseFrame <- tkframe(downFrame)
    downBase <- tclVar(50000)
    tkpack(tkentry(baseFrame, textvariable = downBase, width = 10),
           expand = TRUE, fill = "x", side = "left")
    tkpack(tklabel(baseFrame, text = "bases down"), side = "left",
           expand = FALSE, fill = "x")
    tkpack(baseFrame, expand = FALSE, fill = "x")
    DNFrame <- tkframe(downFrame)
    downNeighbors <- makeViewer(DNFrame, vHeight = 10, vWidth = 15,
                          vScrol = TRUE, hScrol = FALSE, what = "list")
    tkpack(DNFrame, expand = TRUE, fill = "both")
    tkpack(downFrame, side = "left", expand = TRUE, fill = "y",
           padx = 5)
    tkpack(neighborFrame, expand = TRUE, fill = "both")

    butsFrame <- tkframe(rightFrame)
    locatBut <- tkbutton(butsFrame,
                         text = "Locate genes with new settings",
                         command = findUDGenes, state = "disabled")
    collectBut <- tkbutton(butsFrame, text = "Collect data",
                           command = collect, state = "disabled")
    tkgrid(locatBut, collectBut, padx = 8)
    tkpack(butsFrame, expand = FALSE, fill = "x")

    tkpack(rightFrame, padx = 4, expand = TRUE, fill = "both")
    tkpack(conFrame, expand = TRUE, fill = "both")

    exitBut <- tkbutton(base, text = "Exit", width = 20, command = end)
    tkpack(exitBut, pady = 5)

    tkwait.window(base)

    return(collection)
}
