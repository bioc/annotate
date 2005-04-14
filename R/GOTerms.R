### goterms objects are used by homoPkgBuilder to represent homology data

setClass("GOTerms", representation(GOID= "character",
                                   Term = "character",
                                   Synonym = "character",
                                   Secondary = "character",
                                   Definition = "character",
                                   Ontology = "character"))

# Set the get methods
if(!isGeneric("GOID")){
    setGeneric("GOID",
               function(object) standardGeneric("GOID"))
}
setMethod("GOID", "GOTerms",
          function(object) object@GOID)

if(!isGeneric("Term")){
    setGeneric("Term",
               function(object) standardGeneric("Term"))
}
setMethod("Term", "GOTerms",
          function(object) object@Term)

if(!isGeneric("Synonym")){
    setGeneric("Synonym",
               function(object) standardGeneric("Synonym"))
}
setMethod("Synonym", "GOTerms",
          function(object) object@Synonym)

if(!isGeneric("Secondary")){
    setGeneric("Secondary",
               function(object) standardGeneric("Secondary"))
}
setMethod("Secondary", "GOTerms",
          function(object) object@Secondary)

if(!isGeneric("Definition")){
    setGeneric("Definition",
               function(object) standardGeneric("Definition"))
}
setMethod("Definition", "GOTerms",
          function(object) object@Definition)

if(!isGeneric("Ontology")){
    setGeneric("Ontology",
               function(object) standardGeneric("Ontology"))
}
setMethod("Ontology", "GOTerms",
          function(object) object@Ontology)

setMethod("Ontology", signature="ANY", 
   function(object) if(  is.na(object) ) NA else callNextMethod())

setMethod("show", "GOTerms",
          function(object) {
            s <- character(0)
            if(!is.na(GOID(object)))
              s <- c(s, paste("GOID =", GOID(object)), "")
              
            if(!is.na(Term(object)))
              s <- c(s, paste("Term =", Term(object)), "")
              
            if(!is.na(Synonym(object)[1]))
              s <- c(s, paste("\nSynonym =", Synonym(object)), "")
              
            if(!is.na(Secondary(object)[1]))
              s <- c(s, paste("\nSecondary =", Secondary(object)), "")

            if(!is.na(Definition(object)))
              s <- c(s, paste("\nDefinition =", Definition(object)), "")

            if(!is.na(Ontology(object)))
              s <- c(s, paste("\nOntology =", Ontology(object)), "")

            cat(strwrap(s, exdent=5), sep="\n")
})


GOTerms <- function(GOId, term, ontology, synonym = "", secondary = "",
                    definition = ""){
    return(new("GOTerms", GOID = GOId, Term = term,
               Synonym = synonym, Secondary = secondary,
               Definition = definition, Ontology = ontology))
}
