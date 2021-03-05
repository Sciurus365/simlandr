#' Class "hash_big.matrix": big matrix with a md5 hash reference
#'
#' \code{hash_big.matrix} class is a modified class from \code{\link[bigmemory]{big.matrix-class}}. Its purpose is to
#' help users operate big matrices within hard disk in a reusable way, so that the large matrices do not consume
#' too much memory, and the matices can be reused for the next time.
#' Comparing with \code{\link[bigmemory]{big.matrix-class}}, the major enhancement of \code{hash_big.matrix} class
#' is that the backing files are, by default, stored in a permanent place, with the md5 of the object as the file
#' name. With this explicit name, \code{hash_big.matrix} objects can be easily reloaded into workspace every time.
#'
#'
#'
#' @import bigmemory
#' @slot md5 The md5 value of the matrix.
#' @slot address Inherited from \code{big.matrix}.
#'
#' @param x A matrix, vector, or data.frame for \code{\link[bigmemory]{as.big.matrix}}.
#' @param backingpath,... Passed to \code{\link[bigmemory]{as.big.matrix}}.
#' @param silence Suppress messages?
#'
#' @aliases hash_big.matrix
#'
setClass("hash_big.matrix",
				 slots = c(md5 = "character"),
				 # prototype = c(md5 = digest::digest(NULL)), # This line produces fatal errors in RStudio. https://github.com/rstudio/rstudio/issues/8923
				 contains = "big.matrix")


#' @describeIn hash_big.matrix-class Create a \code{hash_big.matrix} object from a matrix.
#' @export
as.hash_big.matrix <- function(x, backingpath = "bp", silence = TRUE, ...){
	if(!dir.exists(backingpath)) dir.create(backingpath)
	temp <- methods::new("hash_big.matrix")
	temp@md5 <- digest::digest(x)
	if(file.exists(file.path(backingpath, paste0(temp@md5, ".desc")))){
		temp@address <- bigmemory::attach.big.matrix(paste0(temp@md5, ".desc"), path = backingpath)@address
		if(!silence) message("Old backing file attached.")
	}else{
		temp@address <- bigmemory::as.big.matrix(x, backingpath = backingpath,
																						 backingfile = paste0(temp@md5, ".bin"),
																						 descriptorfile = paste0(temp@md5, ".desc"))@address
	}
	methods::validObject(temp)
	return(temp)
}

#' @describeIn hash_big.matrix-class Attach a \code{hash_big.matrix} object from the backing file to the workspace.
#' @export
attach.hash_big.matrix <- function(x, backingpath = "bp"){
	if(!methods::is(x, "hash_big.matrix")) stop("Wrong input class. x should be a `hash_big.matrix`.")
	if(!bigmemory::is.nil(x@address)) return(x)
	x@address <- bigmemory::attach.big.matrix(paste0(x@md5, ".desc"), path = backingpath)@address
	return(x)
}
