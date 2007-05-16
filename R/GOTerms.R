### goterms objects are used by homoPkgBuilder to represent homology data

setClass("GOTerms", representation(GOID= "character",
                                   Term = "character",
                                   Synonym = "character",
                                   Secondary = "character",
                                   Definition = "character",
                                   Ontology = "character"))

# Set the get methods: GOID, Term, Synonym, Secondary, Definition and Ontology.
# The generics for these methods are now defined in AnnotationDbi >= 0.0.69
# (H. Pages).

setMethod("GOID", "GOTerms",
          function(object) object@GOID)

setMethod("Term", "GOTerms",
          function(object) object@Term)

setMethod("Synonym", "GOTerms",
          function(object) object@Synonym)

setMethod("Secondary", "GOTerms",
          function(object) object@Secondary)

setMethod("Definition", "GOTerms",
          function(object) object@Definition)

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
