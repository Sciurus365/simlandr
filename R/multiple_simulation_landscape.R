#' Make 3d animations from multiple simulations
#'
#' @param bs A \code{batch_simulation} object created by \code{\link{batch_simulation}.}
#' @param x,y,fr The names of the target variables.
#' \code{fr} corresponds to the \code{frame} parameter in plotly.
#' @param Umax The maximum displayed value of potential.
#' @param n,lims,h,kde_fun Passed to \code{make_2d_kernel_dist}
#' @param individual_landscape Make individual landscape for each simulation?
#' @param mat_3d Also make heatmap matrix?
#'
#' @return A \code{3d_animation_landscape} object that describes the landscape of the system, including the smoothed distribution and the landscape plot.
#'
#' @export
make_3d_animation <- function(bs, x, y, fr, Umax = 5, n = 200, lims = c(-0.1, 1.1, -0.1, 1.1), h = 1e-3, kde_fun = "ks", individual_landscape = FALSE, mat_3d = TRUE) {
  message("Wrangling data...")
  df_multichannel <- bs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dist = list(make_2d_kernel_dist(output, x, y, n, lims, h, kde_fun = kde_fun))
    )

  if (individual_landscape) {
    df_multichannel <- df_multichannel %>%
      dplyr::mutate(
        l_list = list(purrr::quietly(make_3d_static)(output, x = x, y = y, Umax = Umax, n = n, lims = lims, h = h, kde_fun = kde_fun)$result)
      )
  }

  df_multichannel$output <- NULL

  df_multichannel_tidy <- df_multichannel %>%
    dplyr::mutate(tidy_dist = list(make_2d_tidy_dist(dist, !!rlang::sym(fr), var_name = "fr"))) %>%
    dplyr::ungroup()

  df_multichannel_collect <- do.call(rbind, df_multichannel_tidy$tidy_dist)
  message("Done!")

  message("Making the plot...")
  p <-
    df_multichannel_collect %>%
    plotly::plot_ly(x = ~x, y = ~y, z = pmin(-log(.$z %>% t()), Umax), color = pmin(-log(.$z %>% t()), Umax), frame = ~fr) %>%
    plotly::add_markers(size = I(5)) %>%
    plotly::layout(scene = list(xaxis = list(title = x), yaxis = list(title = y), zaxis = list(title = "U"))) %>%
    plotly::animation_slider(
      currentvalue = list(prefix = paste0(fr, ": "))
    )
  message("Done!")

  message("Making the 2d plot...")
  p2 <- ggplot2::ggplot(df_multichannel_collect, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = pmin(-log(z), Umax))) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x = x, y = y, fill = "U") +
    gganimate::transition_states(df_multichannel_collect$fr) +
    ggplot2::labs(subtitle = paste0(fr, ": {closest_state}"))
  message("Done!")

  if (mat_3d) {
    message("Making the 3d matrix...")
    mat_3d <- make_3d_matrix(bs = bs, x = x, y = y, rows = NULL, cols = fr, Umax = Umax, n = n, lims = lims, h = h, kde_fun = kde_fun)
    message("Done!")
  }

  result <- list(dist_raw = df_multichannel, dist = df_multichannel_collect, plot = p, plot_2 = p2, mat_3d = mat_3d, x = x, y = y, fr = fr, Umax = Umax, n = n, lims = lims, h = h, kde_fun = kde_fun)
  class(result) <- c("3d_animation_landscape", "landscape")
  return(result)
}

#' Make a matrix of 2d graphs for two parameters
#' @param bs A \code{batch_simulation} object created by \code{\link{batch_simulation}.}
#' @param x,rows,cols The names of the target variables.
#' If `rows` is `NULL`, only a vector of graphs will be generated.
#' @param adjust,from,to Passed to \code{density}.
#' @param Umax The maximum displayed value of potential.
#' @param individual_landscape Make individual landscape for each simulation?
#'
#' @return A \code{2d_matrix_landscape} object that describes the landscape of the system, including the smoothed distribution and the landscape plot.
#'
#' @export
make_2d_matrix <- function(bs, x, rows = NULL, cols, adjust = 50, from = -0.1, to = 1, Umax = 5, individual_landscape = FALSE) {
  if (is.null(rows)) {
    df_multichannel <- bs %>%
      dplyr::mutate(dist = purrr::map2(output, !!rlang::sym(cols), function(out, sample_var1) {
        d <- stats::density(out[, x], adjust = adjust, from = from, to = to)
        df <- data.frame(x = d$x, y = d$y, U = pmin(-log(d$y), Umax))
        df$cols <- sample_var1
        df
      }))
  } else {
    df_multichannel <- bs %>%
      dplyr::mutate(dist = purrr::pmap(list(output, !!rlang::sym(rows), !!rlang::sym(cols)), function(out, sample_var1, sample_var2) {
        d <- stats::density(out[, x], adjust = adjust, from = from, to = to)
        df <- data.frame(x = d$x, y = d$y, U = pmin(-log(d$y), Umax))
        df$rows <- sample_var1
        df$cols <- sample_var2
        df
      }))
  }
  if (individual_landscape) {
    df_multichannel <- df_multichannel %>%
      dplyr::mutate(
        l_list = purrr::map(output, make_2d_density, x = x, Umax = Umax, adjust = adjust, from = from, to = to)
      )
  }
  df_multichannel$output <- NULL
  df_all <- do.call(rbind, df_multichannel$dist)

  message("Making the plot...")
  rows_labeller <- function(x) paste0(rows, ": ", x)
  cols_labeller <- function(x) paste0(cols, ": ", x)

  p <- df_all %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = U)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::xlab(x)

  if (is.null(rows)) {
    p <- p + ggplot2::facet_wrap(. ~ cols, labeller = ggplot2::labeller(.cols = ggplot2::as_labeller(cols_labeller)))
  } else {
    p <- p + ggplot2::facet_grid(rows ~ cols, labeller = ggplot2::labeller(.rows = ggplot2::as_labeller(rows_labeller), .cols = ggplot2::as_labeller(cols_labeller)))
  }
  message("Done!")

  result <- list(dist_raw = df_multichannel, dist = df_all, plot = p, x = x, rows = rows, cols = cols, adjust = adjust, from = from, to = to, Umax = Umax)
  class(result) <- c("2d_matrix_landscape", "landscape")
  return(result)
}

#' Make a matrix or vector of 3d heatmap graphs for two parameters
#'
#' (Note: a matrix of interactive maps is currently not supported.)
#'
#' @param bs A \code{batch_simulation} object created by \code{\link{batch_simulation}.}
#' @param x,y,rows,cols The names of the target variables.
#' If `rows` is `NULL`, only a vector of graphs will be generated.
#' @param Umax The maximum displayed value of potential.
#' @param n,lims,h,kde_fun Passed to \code{\link{make_2d_kernel_dist}}
#' @param individual_landscape Make individual landscape for each simulation?
#'
#' @return A \code{3d_matrix_landscape} object that describes the landscape of the system, including the smoothed distribution and the landscape plot.
#'
#' @export
make_3d_matrix <- function(bs, x, y, rows = NULL, cols, Umax = 5, n = 200, lims = c(-0.1, 1.1, -0.1, 1.1), h = 1e-3, kde_fun = "ks", individual_landscape = FALSE) {
  if (is.null(rows)) {
    df_multichannel <- bs %>%
      dplyr::mutate(dist = purrr::pmap(list(output, bs[, cols]), function(out, sample_var1) {
        d <- make_2d_kernel_dist(out, x, y, n, lims, h, kde_fun)
        df <- make_2d_tidy_dist(d)
        df$cols <- sample_var1
        df
      }))
  } else {
    df_multichannel <- bs %>%
      dplyr::mutate(dist = purrr::pmap(list(output, bs[, rows], bs[, cols]), function(out, sample_var1, sample_var2) {
        d <- make_2d_kernel_dist(out, x, y, n, lims, h, kde_fun)
        df <- make_2d_tidy_dist(d)
        df$rows <- sample_var1
        df$cols <- sample_var2
        df
      }))
  }

  if (individual_landscape) {
    df_multichannel <- df_multichannel %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        l_list = list(purrr::quietly(make_3d_static)(output, x = x, y = y, Umax = Umax, n = n, lims = lims, h = h, kde_fun = kde_fun)$result)
      ) %>%
      dplyr::ungroup()
  }
  df_multichannel$output <- NULL
  df_all <- do.call(rbind, df_multichannel$dist)

  message("Making the 2d plot...")
  rows_labeller <- function(x) paste0(rows, ": ", x)
  cols_labeller <- function(x) paste0(cols, ": ", x)
  p <- ggplot2::ggplot(data = df_all, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = pmin(-log(z), Umax))) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x = x, y = y, fill = "U")

  if (is.null(rows)) {
    p <- p + ggplot2::facet_wrap(. ~ cols, labeller = ggplot2::labeller(.cols = ggplot2::as_labeller(cols_labeller)))
  } else {
    p <- p + ggplot2::facet_grid(rows ~ cols, labeller = ggplot2::labeller(.rows = ggplot2::as_labeller(rows_labeller), .cols = ggplot2::as_labeller(cols_labeller)))
  }
  message("Done!")

  result <- list(dist_raw = df_multichannel, dist = df_all, plot = p, x = x, y = y, rows = rows, cols = cols, Umax = Umax, n = n, lims = lims, h = h, kde_fun = kde_fun)
  class(result) <- c("3d_matrix_landscape", "landscape")
  return(result)
}
