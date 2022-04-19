#' Class "hash_big_matrix": big matrix with a md5 hash reference
#'
#' `hash_big_matrix` class is a modified class from [bigmemory::big.matrix-class()]. Its purpose is to
#' help users operate big matrices within hard disk in a reusable way, so that the large matrices do not consume
#' too much memory, and the matrices can be reused for the next time.
#' Comparing with [bigmemory::big.matrix-class()], the major enhancement of `hash_big_matrix` class
#' is that the backing files are, by default, stored in a permanent place, with the md5 of the object as the file
#' name. With this explicit name, `hash_big_matrix` objects can be easily reloaded into workspace every time.
#'
#'
#'
#' @import bigmemory
#' @slot md5 The md5 value of the matrix.
#' @slot address Inherited from `big.matrix`.
#'
#' @param x A matrix, vector, or data.frame for [bigmemory::as.big.matrix()].
#' @param backingpath,... Passed to [bigmemory::as.big.matrix()].
#' @param silence Suppress messages?
#'
#' @aliases hash_big_matrix
#'
setClass("hash_big_matrix",
  slots = c(md5 = "character"),
  # prototype = c(md5 = digest::digest(NULL)), # This line produces fatal errors in RStudio. https://github.com/rstudio/rstudio/issues/8923
  contains = "big.matrix"
)


#' @describeIn hash_big_matrix-class Create a `hash_big_matrix` object from a matrix.
#' @export
as_hash_big_matrix <- function(x, backingpath = "bp", silence = TRUE, ...) {
  if (!dir.exists(backingpath)) dir.create(backingpath)
  temp <- methods::new("hash_big_matrix")
  temp@md5 <- digest::digest(x)
  if (file.exists(file.path(backingpath, paste0(temp@md5, ".desc")))) {
    temp@address <- bigmemory::attach.big.matrix(paste0(temp@md5, ".desc"), path = backingpath)@address
    if (!silence) message("Old backing file attached.")
  } else {
    temp@address <- bigmemory::as.big.matrix(x,
      backingpath = backingpath,
      backingfile = paste0(temp@md5, ".bin"),
      descriptorfile = paste0(temp@md5, ".desc")
    )@address
  }
  methods::validObject(temp)
  return(temp)
}

#' @describeIn hash_big_matrix-class Attach a `hash_big_matrix` object from the backing file to the workspace.
#' @export
attach_hash_big_matrix <- function(x, backingpath = "bp") {
  if (!methods::is(x, "hash_big_matrix")) stop("Wrong input class. x should be a `hash_big_matrix`.")
  if (!bigmemory::is.nil(x@address)) {
    return(x)
  }
  x@address <- bigmemory::attach.big.matrix(paste0(x@md5, ".desc"), path = backingpath)@address
  return(x)
}
