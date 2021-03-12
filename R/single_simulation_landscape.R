#' Make 2D density-based landscape plot for a single simulation output
#'
#' @param output A matrix of simulation output.
#' @param x The name of the target variable.
#' @param adjust,from,to Passed to \code{density}.
#' @param zmax The maximum displayed value of potential.
#'
#' @return A \code{2d_density_landscape} object.
#' @export
make_2d_density <- function(output, x, adjust = 50, from = -0.1, to = 1, zmax = 5) {
  d <- stats::density(output[, x], adjust = adjust, from = from, to = to)
  p <- data.frame(x = d$x, y = d$y, U = pmin(-log(d$y), zmax)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = U)) +
    ggplot2::geom_line() +
    # geom_smooth(se = F) +
    ggplot2::theme_bw() +
    ggplot2::xlab(x)

  result <- list(dist = d, plot = p, x = x, adjust = adjust, from = from, to = to, zmax = zmax)
  class(result) <- c("2d_density_landscape", "landscape")
  return(result)
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
#' @param n,lims,h Passed to \code{\link[ks]{kde}} or \code{\link[MASS]{kde2d}}.
#' If using \code{ks::kde}, \code{H = diag(h, 2, 2)}.
#' Note: the definition of bandwidth (`h`) is different in two functions.
#' To get a similar output, the `h` is about 50 to 5000 times smaller for \code{\link[ks]{kde}} than \code{\link[MASS]{kde2d}}
#' @param kde_fun Which to use? Choices: "ks" \code{ks::kde} (default; faster and taking less memory); "MASS" \code{MASS::kde2d}.
#'
#' @return A \code{kde2d}-type list.
#' @export
make_kernel_dist <- function(output, x, y, n = 200, lims = c(-0.1, 1.1, -0.1, 1.1), h, kde_fun = "ks") {
  if (is.list(output)) output <- output[[1]]
  if (any(!is.finite(output[, x])) || any(!is.finite(output[, y]))) {
    return(NULL)
  }
  if (kde_fun == "MASS") {
    return(MASS::kde2d(x = output[, x], y = output[, y], n = n, lims = lims, h = h))
  } else if (kde_fun == "ks") {
    # prepare the parameters for ks::kde
    output_x <- output[, c(x, y)]
    if (!missing(h)) H <- diag(h, 2, 2)
    gridsize <- rep(n, 2)
    xmin <- lims[c(1, 3)]
    xmax <- lims[c(2, 4)]

    # calculate the result using ks::kde
    if (!missing(h)) {
      result <- ks::kde(output_x, H = H, gridsize = gridsize, xmin = xmin, xmax = xmax, compute.cont = FALSE, approx.cont = FALSE)
    } else {
      result <- ks::kde(output_x, gridsize = gridsize, xmin = xmin, xmax = xmax, compute.cont = FALSE, approx.cont = FALSE)
    }
    # reformat the result to the format of MASS::kde2d
    result <- list(x = result$eval.points[[1]], y = result$eval.points[[2]], z = pmax(result$estimate, 0)) # different result??
    return(result)
  }
  else {
    stop('Wrong input for `kde_fun`. Please choose from "MASS" and "ks".')
  }
}


#' Make 3D static landscape plots from simulation output
#'
#' @param output A matrix of simulation output.
#' @param x,y The name of the target variable.
#' @param zmax The maximum displayed value of potential.
#' @param n,lims,h,kde_fun Passed to \code{\link{make_kernel_dist}}
#'
#' @return A \code{3d_static_landscape}, \code{landscape} object.
#'
#' @export
make_3d_static <- function(output, x, y, zmax = 5, n = 200, lims = c(-0.1, 1.1, -0.1, 1.1), h = 1e-3, kde_fun = "ks") {
  if (is.list(output)) output <- unlist(output)

  message("Calculating the smooth distribution...")
  out_2d <- make_kernel_dist(output, x, y, n, lims, h, kde_fun)
  message("Done!")

  message("Making the plot...")
  p <- plotly::plot_ly(x = out_2d$x, y = out_2d$y, z = pmin(-log(out_2d$z), zmax), type = "surface")
  p <- plotly::layout(p, scene = list(xaxis = list(title = x), yaxis = list(title = y), zaxis = list(title = "U")))
  message("Done!")

  message("Making the 2d plot...")
  p2 <- ggplot2::ggplot(make_tidy_dist(out_2d), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = pmin(-log(z), zmax))) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x = x, y = y, fill = "U")
  message("Done!")

  result <- list(dist = out_2d, plot = p, plot_2 = p2, x = x, y = y, zmax = zmax, n = n, lims = lims, h = h, kde_fun = kde_fun)
  class(result) <- c("3d_static_landscape", "landscape")
  return(result)
}
