serializeEnv <- function(env, fname, asList=TRUE) {
 if (!is.character(fname))
     stop("conn should be a character name of file for storage")

 if (asList)
     env <- as.list(env)

 .saveRDS(env, fname)
}

