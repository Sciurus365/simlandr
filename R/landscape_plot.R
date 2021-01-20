#' Make plots from landscape objects
#'
#' @param l A landscape object
#'
#' @export
plot.landscape <- function(x, ...) {x$plot}

#' Save landscape plots
#'
#' @param l A landscape object
#' @param path The path to save the output. Default: "/pics/x_y.html".
#' @param selfcontained For plotly plots, save the output as a selfcontained html file? Default: FALSE.
#' @param ... Other parameters passed to \code{\link[htmlwidgets]{saveWidget}}
#' or \code{\link[ggplot2]{ggsave}}
#' @export
save_landscape <- function(l, path = NULL, selfcontained = F, ...){
	p <- l$plot
	message("Saving the plot...")
	if(is.null(path)){
		if(!is.null(l$fr)) path <- paste(getwd(), "/pics/", l$x, "_", l$y, "_", l$fr, ".html", sep = "")
		else path <- paste(getwd(), "/pics/", l$x, "_", l$y, sep = "")
	}

	if("plotly" %in% class(p)) htmlwidgets::saveWidget(p, paste(path, ".html", sep = ""), selfcontained = selfcontained, ...)
	else{
		if("ggplot" %in% class(p)) ggplot2::ggsave(paste(path, ".png", sep = ""), p, ...)
	}
	message("Done!")
}
