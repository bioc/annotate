.First.lib <- function(libname, pkgname, where) {
  if( version$major == "1" && as.numeric(version$minor) < 4 )
    require(RSMethods)
  else
    require(methods)
   if(missing(where)) {
        where <- match(paste("package:", pkgname, sep=""), search())
        if(is.na(where)) {
            warning(paste("Not a package name: ",pkgname))
            return()
        }
        where <- pos.to.env(where)
    }
   .initClasses(where)
}
