### homoData objects are used by homoPkgBuilder to represent homology data

setClass("homoData", representation(homoOrg = "character",
                                    homoLL = "numeric",
                                    homoType = "character",
                                    homoPS = "numeric",
                                    homoURL = "character",
                                    homoACC = "character",
                                    homoHGID = "numeric"))

# Set the get methods
if(!isGeneric("homoOrg")){
    setGeneric("homoOrg",
               function(object) standardGeneric("homoOrg"))
}
setMethod("homoOrg", "homoData",
          function(object) object@homoOrg)

if(!isGeneric("homoLL")){
    setGeneric("homoLL",
               function(object) standardGeneric("homoLL"))
}
setMethod("homoLL", "homoData",
          function(object) object@homoLL)

if(!isGeneric("homoType")){
    setGeneric("homoType",
               function(object) standardGeneric("homoType"))
}
setMethod("homoType", "homoData",
          function(object) object@homoType)

if(!isGeneric("homoPS")){
    setGeneric("homoPS",
               function(object) standardGeneric("homoPS"))
}
setMethod("homoPS", "homoData",
          function(object) object@homoPS)

if(!isGeneric("homoURL")){
    setGeneric("homoURL",
               function(object) standardGeneric("homoURL"))
}
setMethod("homoURL", "homoData",
          function(object) object@homoURL)

if(!isGeneric("homoACC")){
    setGeneric("homoACC",
               function(object) standardGeneric("homoACC"))
}
setMethod("homoACC", "homoData",
          function(object) object@homoACC)

if(!isGeneric("homoHGID")){
    setGeneric("homoHGID",
               function(object) standardGeneric("homoHGID"))
}
setMethod("homoHGID", "homoData",
          function(object) object@homoHGID)

# Print the contents of a homoPS object
if(!isGeneric("print")){
    setGeneric("print",
               function(x, ...) standardGeneric("print"))
}
setMethod("print", "homoData",
          function(x, ...) {
               print(paste("homoOrg:", homoOrg(x)))
               print(paste("homoLL:", homoLL(x)))
               print(paste("homoHGID:", homoHGID(x)))
               print(paste("homoACC:", homoACC(x)))
               print(paste("homoType:", homoType(x)))
               print(paste("homoPS:", homoPS(x)))
               print(paste("homoURL:", homoURL(x)))
           })


homoData <- function(organism, LL, type, PS, ACC, HGID, URL){
    return(new("homoData", homoOrg = mapOrgs(organism),
                   homoLL = LL, homoType = type,
                   homoPS = PS, homoURL = URL,
                   homoACC = ACC, homoHGID = HGID))
}
