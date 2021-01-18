#' Make plots from landscape objects
#'
#' @param l A landscape object
#'
#' @export
plot.landscape <- function(l, ...) {l$plot}

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
		if(is.null(l$var)) path <- paste(getwd(), "/pics/", x, "_", y, "_", var, ".html", sep = "")
		else path <- paste(getwd(), "/pics/", x, "_", y, ".html", sep = "")
	}

	if("plotly" %in% class(p)) htmlwidgets::saveWidget(p, path, selfcontained = selfcontained, ...)
	else{
		if("ggplot" %in% class(p)) ggplot2::ggsave(path, p, ...)
	}
	message("Done!")
}
