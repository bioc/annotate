### goterms objects are used by homoPkgBuilder to represent homology data

setClass("GOTerms", representation(GOId = "character",
                                   Ontology = "character",
                                   Synonym = "character",
                                   Secondary = "character",
                                   Definition = "character",
                                   Category = "character"))

# Set the get methods
if(!isGeneric("GOId")){
    setGeneric("GOId",
               function(object) standardGeneric("GOId"))
}
setMethod("GOId", "GOTerms",
          function(object) object@GOId)

if(!isGeneric("Ontology")){
    setGeneric("Ontology",
               function(object) standardGeneric("Ontology"))
}
setMethod("Ontology", "GOTerms",
          function(object) object@Ontology)

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
               print(paste("GOId =", GOId(x)))
               print(paste("GO ontology term =", Ontology(x)))
               print(paste("Synonymous terms =", Synonym(x)))
               print(paste("Secondary GO ids =", Secondary(x)))
               print(paste("Definition for GO term =", Definition(x)))
               print(paste("GO category =", Category(x)))
           })


GOTerms <- function(GOId, ontology, category, synonym = "", secondary = "",
                    definition = ""){
    return(new("GOTerms", GOId = GOId, Ontology = ontology,
               Synonym = synonym, Secondary = secondary,
               Definition = definition, Category = category))
}
