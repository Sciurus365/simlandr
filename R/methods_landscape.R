#' Get the probability distribution from a landscape object
#'
#' @param l A \code{landscape} project.
#' @param index 1 to get the distribution in tidy format; 2 or "raw" to get the raw simulation result (\code{batch_simulation}).
#'
#' @export
get_dist <- function(l, index = 1){
	if(!"landscape" %in% class(l)) stop("l should be a `landscape`")
	if(index == 1) return(l$dist)
	if(index == 2 | index == "raw") return(l$dist_raw)
}
