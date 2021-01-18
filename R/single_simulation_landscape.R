#' Make 2D density-based landscape plot for a single simulation output
#'
#' @param output A matrix of simulation output.
#' @param x The name of the target variable.
#' @param adjust,from,to Passed to \code{density}.
#' @param zmax The maximum displayed value of potential.
#'
#' @return A \code{ggplot} graph.
#' @export
make_2d_density <- function(output, x, adjust = 50, from = -0.1, to = 1, zmax = 5) {
  d <- stats::density(output[, x], adjust = adjust, from = from, to = to)
  data.frame(x = d$x, y = d$y, U = pmin(-log10(d$y), zmax)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = U)) +
    ggplot2::geom_line() +
    # geom_smooth(se = F) +
    ggplot2::theme_bw() +
    ggplot2::xlab(x)
}

#' A function for reversed log transformation
#'
#' @param base The base of logarithm
#'
#' @export
reverselog_trans <- function(base = exp(1)) {
  force(base)
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
    scales::log_breaks(base = base),
    domain = c(1e-100, Inf)
  )
}

#' Make 2D kernel smooth distribution
#'
#' @param output A matrix of simulation output.
#' @param x,y The name of the target variable.
#' @param n,lims,h Passed to \code{\link[MASS]{kde2d}}
#'
#' @return A \code{kde2d}-type list.
#' @export
make_kernel_dist <- function(output, x, y, n = 200, lims = c(-0.1, 1.1, -0.1, 1.1), h = 0.1) {
  if (is.list(output)) output <- output[[1]]
  data_x <- output[, x]
  data_y <- output[, y]
  if (any(!is.finite(data_x)) || any(!is.finite(data_y))) {
    return(NULL)
  }
  return(MASS::kde2d(x = data_x, y = data_y, n = n, lims = lims, h = h))
}

#' Make 3D static landscape plots from simulation output
#'
#' @param output A matrix of simulation output.
#' @param x,y The name of the target variable.
#' @param zmax The maximum displayed value of potential.
#' @param save Save the output as a html file? Default: FALSE.
#' @param path The path to save the output. Default: "/pics/x_y.html".
#' @param selfcontained Save the output as a selfcontained html file? Default: FALSE.
#' @param n,lims,h Passed to \code{\link[MASS]{kde2d}}
#'
#' @return A \code{3d_static_landscape}, \code{landscape} object.
#'
#' @export
make_3d_static <- function(output, x, y, zmax = 5, save = FALSE, path = NULL, selfcontained = F, n = 200, lims = c(-0.1, 1.1, -0.1, 1.1), h = 0.1) {
  if (is.list(output)) output <- unlist(output)

  message("Calculating the smooth distribution...")
  out_2d <- make_kernel_dist(output, x, y, n, lims, h)
  message("Done!")

  message("Making the plot...")
  p <- plotly::plot_ly(x = out_2d$x, y = out_2d$y, z = pmin(-log10(out_2d$z), zmax), type = "surface")
  p <- plotly::layout(p, scene = list(xaxis = list(title = x), yaxis = list(title = y), zaxis = list(title = "U")))
  message("Done!")

  if (save) {
    message("Saving the plot...")
    if (is.null(path)) {
      path <- paste(getwd(), "/pics/", x, "_", y, ".html", sep = "")
    }
    htmlwidgets::saveWidget(p, path, selfcontained = selfcontained)
    message("Done!")
  }

  result <- list(dist = out_2d, plot = p, x = x, y = y)
  class(result) <- c("3d_static_landscape", "landscape")
  return(result)
}
