### goterms objects are used by homoPkgBuilder to represent homology data

setClass("GOTerms", representation(GOID= "character",
                                   Term = "character",
                                   Synonym = "character",
                                   Secondary = "character",
                                   Definition = "character",
                                   Category = "character"))

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

if(!isGeneric("Category")){
    setGeneric("Category",
               function(object) standardGeneric("Category"))
}
setMethod("Category", "GOTerms",
          function(object) object@Category)

setMethod("print", "GOTerms",
          function(x, ...) {
               print("An object of class GOTerms")
               print(paste("GOID =", GOID(x)))
               print(paste("Gene Ontology term =", Term(x)))
               print(paste("Synonymous terms =", Synonym(x)))
               print(paste("Secondary GO ids =", Secondary(x)))
               print(paste("Definition for GO term =", Definition(x)))
               print(paste("GO category =", Category(x)))
           })


GOTerms <- function(GOId, term, category, synonym = "", secondary = "",
                    definition = ""){
    return(new("GOTerms", GOID = GOId, Term = term,
               Synonym = synonym, Secondary = secondary,
               Definition = definition, Category = category))
}
