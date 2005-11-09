### homoData objects are used by homoPkgBuilder to represent homology data

setClass("homoData", representation(homoOrg = "character",
                                    homoLL = "numeric",
                                    homoType = "character",
                                    homoPS = "numeric",
                                    homoURL = "character",
                                    homoACC = "character",
                                    homoHGID = "numeric"))

# Set the get methods
setGeneric("homoOrg",
               function(object) standardGeneric("homoOrg"))

setMethod("homoOrg", "homoData",
          function(object) object@homoOrg)

setGeneric("homoLL",
               function(object) standardGeneric("homoLL"))

setMethod("homoLL", "homoData",
          function(object) object@homoLL)

setGeneric("homoType",
               function(object) standardGeneric("homoType"))

setMethod("homoType", "homoData",
          function(object) object@homoType)

setGeneric("homoPS",
               function(object) standardGeneric("homoPS"))

setMethod("homoPS", "homoData",
          function(object) object@homoPS)

setGeneric("homoURL",
               function(object) standardGeneric("homoURL"))

setMethod("homoURL", "homoData",
          function(object) object@homoURL)

setGeneric("homoACC",
               function(object) standardGeneric("homoACC"))

setMethod("homoACC", "homoData",
          function(object) object@homoACC)

setGeneric("homoHGID",
               function(object) standardGeneric("homoHGID"))

setMethod("homoHGID", "homoData",
          function(object) object@homoHGID)

setMethod("show", "homoData",
          function(object) {
              if(length(homoOrg(object)) > 0 && !is.na(homoOrg(object))){
                  cat(paste("homoOrg:", homoOrg(object)), fill = TRUE)
              }
              if(length(homoLL(object)) > 0 && !is.na(homoLL(object))){
                  cat(paste("\nhomoLL:", homoLL(object)), fill = TRUE)
              }
              if(length(homoHGID(object)) > 0 && !is.na(homoHGID(object))){
                  cat(paste("\nhomoHGID:", homoHGID(object)), fill = TRUE)
              }
              if(length(homoACC(object)) > 0 && !is.na(homoACC(object))){
                  cat(paste("\nhomoACC:", homoACC(object)), fill = TRUE)
              }
              if(length(homoType(object)) > 0 && !is.na(homoType(object))){
                  cat(paste("\nhomoType:", homoType(object)), fill = TRUE)
              }
              if(length(homoPS(object)) > 0 && !is.na(homoPS(object))){
                  cat(paste("\nhomoPS:", homoPS(object)), fill = TRUE)
              }
              if(length(homoURL(object)) > 0 && !is.na(homoURL(object))){
                  cat(paste("\nhomoURL:", homoURL(object)), fill = TRUE)
              }
              cat("\n")
})


homoData <- function(organism, LL, type, PS, ACC, HGID, URL){
    return(new("homoData", homoOrg = mapOrgs(organism),
                   homoLL = LL, homoType = type,
                   homoPS = PS, homoURL = URL,
                   homoACC = ACC, homoHGID = HGID))
}
