sourceandgetvar <- function(filename, varname) {
  direct = gsub("/R","",getwd())
  setwd(paste(direct,"/R",sep=""))
  ff <- new.env()
  sys.source(filename, ff)
  stopifnot(varname %in% ls(envir=ff))
  ff[[varname]]
}