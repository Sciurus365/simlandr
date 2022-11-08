#' @export
#' @method print landscape
print.landscape <- function(x, ...) {
  cat("A landscape object of the class", class(x)[1], "was estimated. Use `plot()` to draw the landscape plot.")
}

#' Make plots from landscape objects
#'
#' @param x A landscape object
#' @param index Default is 1. For some landscape objects, there is a second plot (usually 2d heatmaps for 3d landscapes)
#' or a third plot (usually 3d matrices for 3d animations).
#' Use `index = 2` to plot that one.
#' @param ... Not in use.
#'
#' @return The plot.
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
#' @param selfcontained For 'plotly' plots, save the output as a self-contained html file? Default: FALSE.
#' @param ... Other parameters passed to [htmlwidgets::saveWidget()]
#' or [ggplot2::ggsave()]
#'
#' @return The function saves the plot to a specific path. It does not have a return value.
#' @export
save_landscape <- function(l, path = NULL, selfcontained = FALSE, ...) {
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
  return(NULL)
}

#' Get the probability distribution from a landscape object
#'
#' @param l A `landscape` project.
#' @param index 1 to get the distribution in tidy format; 2 or "raw" to get the raw simulation result (`batch_simulation`).
#'
#' @return A `data.frame` that contains the distribution in the tidy format or the raw simulation result.
#' @export
get_dist <- function(l, index = 1) {
  if (!"landscape" %in% class(l)) stop("l should be a `landscape`")
  if (index == 1) {
    return(l$dist)
  }
  if (index == 2 | index == "raw") {
    return(l$dist_raw)
  }
}
