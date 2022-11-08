#' Make 2D static landscape plot for a single simulation output
#' @param x The name of the target variable.
#' @inheritParams make_kernel_dist
#' @return A `2d_static_landscape` object that describes the landscape of the system, including the smooth distribution and the landscape plot.
#' @export
make_2d_static <- function(output, x, lims, kde_fun = c("ks", "base"), n = 200, h, adjust = 1, Umax = 5) {
  if (is.list(output)) output <- output[[1]]
  kde_fun <- kde_fun[1]

  var_names <- x
  h <- determine_h(output, var_names, kde_fun, h %>% rlang::maybe_missing(), adjust)
  lims <- determine_lims(output, var_names, lims)

  d <- make_kernel_dist(output, var_names, lims, kde_fun, n, h, adjust)

  p <- data.frame(x = d$x, y = d$d, U = pmin(-log(d$d), Umax)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = U)) +
    ggplot2::geom_line() +
    # geom_smooth(se = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::xlab(x)

  result <- list(dist = d, plot = p, x = x, lims = lims, kde_fun = kde_fun, n = n, h = h, adjust = adjust, Umax = Umax)
  class(result) <- c("2d_static_landscape", "2d_landscape", "landscape")
  return(result)
}

#' Make 3D static landscape plots from simulation output
#'
#' @param x,y The names of the target variables.
#' @inheritParams make_kernel_dist
#'
#' @return A `3d_static_landscape` object that describes the landscape of the system, including the smooth distribution and the landscape plot.
#'
#' @export
make_3d_static <- function(output, x, y, lims, kde_fun = c("ks", "MASS"), n = 200, h, adjust = 1, Umax = 5) {
  if (is.list(output)) output <- output[[1]]
  kde_fun <- kde_fun[1]

  var_names <- c(x,y)
  h <- determine_h(output, var_names, kde_fun, h %>% rlang::maybe_missing(), adjust)
  lims <- determine_lims(output, var_names, lims)

  out_2d <- make_kernel_dist(output, var_names, lims, kde_fun, n, h, adjust)

  p <- plotly::plot_ly(x = out_2d$x, y = out_2d$y, z = pmin(-log(out_2d$d %>% t()), Umax), type = "surface")
  p <- plotly::layout(p, scene = list(xaxis = list(title = x), yaxis = list(title = y), zaxis = list(title = "U"))) %>% plotly::colorbar(title = "U")

  p2 <- ggplot2::ggplot(make_2d_tidy_dist(out_2d), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = pmin(-log(d), Umax))) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x = x, y = y, fill = "U") +
    ggplot2::theme_bw()

  result <- list(dist = out_2d, plot = p, plot_2 = p2, x = x, y = y, lims = lims, kde_fun = kde_fun, n = n, h = h, adjust = adjust, Umax = Umax)
  class(result) <- c("3d_static_landscape", "3d_landscape", "landscape")
  return(result)
}


#' Make 4D static space-color plots from simulation output
#'
#' @param x,y,z The names of the target variables.
#' @inheritParams make_kernel_dist
#'
#' @return A `4d_static_landscape` object that describes the landscape of the system, including the smoothed distribution and the landscape plot.
#'
#' @export
make_4d_static <- function(output, x, y, z, lims, kde_fun = "ks", n = 50, h, adjust = 1, Umax = 5) {
  if (is.list(output)) output <- output[[1]]
  kde_fun <- kde_fun[1]

  var_names <- c(x,y,z)
  h <- determine_h(output, var_names, kde_fun, h %>% rlang::maybe_missing(), adjust)
  lims <- determine_lims(output, var_names, lims)

  out_3d <- make_kernel_dist(output, var_names, lims, kde_fun, n, h, adjust)

  df_tidy <- out_3d %>% make_3d_tidy_dist()

  p <-
    df_tidy %>%
    dplyr::filter(-log(.$d) < Umax) %>%
    plotly::plot_ly(x = ~x, y = ~y, z = ~z, color = -log(.$d)) %>%
    plotly::add_markers(size = I(5)) %>%
    plotly::layout(scene = list(xaxis = list(title = x), yaxis = list(title = y), zaxis = list(title = z))) %>%
    plotly::colorbar(title = "U")

  result <- list(dist = df_tidy, plot = p, x = x, y = y, lims = lims, kde_fun = kde_fun, n = n, h = h, adjust = adjust, Umax = Umax)
  class(result) <- c("4d_static_landscape", "4d_landscape", "landscape")
  return(result)
}
