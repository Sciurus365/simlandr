#' Make plots from landscape objects
#'
#' @param x A landscape object
#' @param index Default is 1. For some landscape objects, there is a second plot (usually 2d heatmaps for 3d landscapes)
#' or a third plot (usually 3d matrices for 3d animations).
#' Use `index = 2` to plot that one.
#' @param ... Not in use.
#'
#' @export
plot.landscape <- function(x, index = 1, ...) {
  if (index == 1) {
    x$plot
  } else if (index == 2) {
    x$plot_2
  } else if (index == "mat_3d" | index == 3) plot(x$mat_3d)
}

#' Save landscape plots
#'
#' @param l A landscape object
#' @param path The path to save the output. Default: "/pics/x_y.html".
#' @param selfcontained For plotly plots, save the output as a selfcontained html file? Default: FALSE.
#' @param ... Other parameters passed to \code{\link[htmlwidgets]{saveWidget}}
#' or \code{\link[ggplot2]{ggsave}}
#' @export
save_landscape <- function(l, path = NULL, selfcontained = F, ...) {
  p <- l$plot
  message("Saving the plot...")
  if (is.null(path)) {
    if (!is.null(l$fr)) {
      path <- paste(getwd(), "/pics/", l$x, "_", l$y, "_", l$fr, ".html", sep = "")
    } else {
      path <- paste(getwd(), "/pics/", l$x, "_", l$y, sep = "")
    }
  }

  if ("plotly" %in% class(p)) {
    htmlwidgets::saveWidget(p, paste(path, ".html", sep = ""), selfcontained = selfcontained, ...)
  } else {
    if ("ggplot" %in% class(p)) ggplot2::ggsave(paste(path, ".png", sep = ""), p, ...)
  }
  message("Done!")
}
