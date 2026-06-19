#' @export
#' @method print landscape
print.landscape <- function(x, ...) {
  cat(
    "A landscape object of the class", class(x)[1],
    "was estimated. Use `autoplot()` for ggplot output or `plotly_ld()`",
    "for an interactive landscape."
  )
}

#' Autoplot landscape objects
#'
#' Returns the ggplot representation stored in a landscape object.
#'
#' @param object A landscape object.
#' @param ... Not in use.
#'
#' @return A ggplot object.
#' @importFrom ggplot2 autoplot
#' @export
autoplot.landscape <- function(object, ...) {
  if (ggplot2::is_ggplot(object$plot_2)) {
    return(object$plot_2)
  }
  if (ggplot2::is_ggplot(object$plot)) {
    return(object$plot)
  }
  cli::cli_abort("This landscape object does not contain a ggplot representation.")
}

#' Plot landscape objects interactively
#'
#' Returns the plotly representation stored in a landscape object.
#'
#' @param object A landscape object.
#' @param ... Not in use.
#'
#' @return A plotly object.
#' @export
plotly_ld <- function(object, ...) {
  UseMethod("plotly_ld")
}

#' @export
plotly_ld.default <- function(object, ...) {
  cli::cli_abort(
    "No {.fn plotly_ld} method is available for objects of class {.cls {class(object)[1]}}."
  )
}

#' @export
plotly_ld.landscape <- function(object, ...) {
  if (!inherits(object$plot, "plotly")) {
    cli::cli_abort("This landscape object does not contain a plotly representation.")
  }
  object$plot
}

#' Plot landscape objects
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Use [ggplot2::autoplot()] for ggplot output or [plotly_ld()] for
#' interactive landscape output.
#'
#' @param x A landscape object
#' @param index Default is 1. For some landscape objects, there is a second plot (usually 2d heatmaps for 3d landscapes)
#' or a third plot (usually 3d matrices for 3d animations).
#' Use `index = 2` to plot that one.
#' @param ... Arguments passed to the replacement method.
#'
#' @return The plot.
#'
#' @export
plot.landscape <- function(x, index = 1, ...) {
  if (index == 1) {
    replacement <- if (inherits(x$plot, "plotly")) {
      "plotly_ld()"
    } else {
      "ggplot2::autoplot()"
    }
    lifecycle::deprecate_warn("0.4.1", "plot.landscape()", replacement)
    if (inherits(x$plot, "plotly")) {
      plotly_ld(x, ...)
    } else {
      ggplot2::autoplot(x, ...)
    }
  } else if (index == 2) {
    lifecycle::deprecate_warn(
      "0.4.1",
      "plot.landscape()",
      "ggplot2::autoplot()"
    )
    ggplot2::autoplot(x, ...)
  } else if (index == "mat_3d" | index == 3) {
    lifecycle::deprecate_warn(
      "0.4.1",
      "plot.landscape()",
      "ggplot2::autoplot()"
    )
    ggplot2::autoplot(x$mat_3d, ...)
  } else {
    cli::cli_abort("{.arg index} must be 1, 2, 3, or {.val mat_3d}.")
  }
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
  cli::cli_inform("Saving the plot...")
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
  cli::cli_inform("Done!")
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
  if (!"landscape" %in% class(l)) {
    cli::cli_abort("{.arg l} must be a {.cls landscape} object.")
  }
  if (index == 1) {
    return(l$dist)
  }
  if (index == 2 | index == "raw") {
    return(l$dist_raw)
  }
}

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot
