serializeEnv <- function(env, fname) {
 if (!is.character(fname))
     stop("conn should be a character name of file for storage")

 envLst <- as.list(env)
 .saveRDS(envLst, fname)

 length(envLst)
}

