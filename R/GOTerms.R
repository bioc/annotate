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

setMethod("show", "GOTerms",
          function(object) {
              if(!is.na(GOID(object))){
                  cat(paste("GOID =", GOID(object)), fill = TRUE)
              }
              if(!is.na(Term(object))){
                  cat(paste("\nTerm =", Term(object)), fill = TRUE)
              }
              if(!is.na(Synonym(object)[1])){
                  cat(paste("\nSynonym =", Synonym(object)), fill = TRUE)
              }
              if(!is.na(Secondary(object)[1])){
                  cat(paste("\nSecondary =", Secondary(object)),
                      fill = TRUE)
              }
              if(!is.na(Definition(object))){
                  cat(paste("\nDefinition =", Definition(object)),
                      fill = TRUE)
              }
              if(!is.na(Ontology(object))){
                  cat(paste("\nOntology =", Ontology(object)), fill = TRUE)
              }
              cat("\n")
})


GOTerms <- function(GOId, term, ontology, synonym = "", secondary = "",
                    definition = ""){
    return(new("GOTerms", GOID = GOId, Term = term,
               Synonym = synonym, Secondary = secondary,
               Definition = definition, Ontology = ontology))
}
