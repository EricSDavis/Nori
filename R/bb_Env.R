bbEnv <- new.env()



.onLoad <- function(libname, pkgname){

  assign("variable", 2, envir = parent.env(environment()))


}







