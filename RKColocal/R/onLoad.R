#' @title Load Jar Documents
#' 
#' @description this function help package find Jar documents
#' 
#' @details Depends on \code{rJava} package mainly.
#' 
#' @param libname a string
#' @param pkgname a string
#' 
#' @return \code{NULL}
#' 
#' @import rJava

.onLoad<-function (libname, pkgname) {
     .jpackage(pkgname, lib.loc = libname)
}
