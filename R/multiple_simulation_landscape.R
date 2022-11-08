#' Make a matrix of 2D static landscape plots for one or two parameters
#' @param bs A `batch_simulation` object created by `[batch_simulation()].`
#' @inheritParams make_2d_static
#' @param rows,cols The names of the parameters. `rows` can be left blank if only one parameter is needed.
#' @param individual_landscape Make individual landscape for each simulation? Default is `TRUE` so that it is possible to calculate barriers. Set to `FALSE` to save time.
#'
#' @return A `2d_matrix_landscape` object that describes the landscape of the system, including the smoothed distribution and the landscape plot.
#'
#' @export
make_2d_matrix <- function(bs, x, rows = NULL, cols, lims, kde_fun = c("ks", "base"), n = 200, h, adjust = 1, Umax = 5, individual_landscape = TRUE) {
	kde_fun <- kde_fun[1]
	var_names <- x
	h <- determine_h_batch(bs, var_names, kde_fun, h %>% rlang::maybe_missing(), adjust)
	lims <- determine_lims_batch(bs, var_names, lims %>% rlang::maybe_missing())

	message("Wrangling the data...")
  if (is.null(rows)) {
    df_nested <- bs %>%
      dplyr::mutate(dist = purrr::map2(output, !!rlang::sym(cols), function(out, par_value1) {
        d <- make_kernel_dist(out, var_names, lims, kde_fun, n, h, adjust)
        df <- data.frame(x = d$x, d = d$d, U = pmin(-log(d$d), Umax))
        df$cols <- par_value1
        df
      }))
  } else {
    df_nested <- bs %>%
      dplyr::mutate(dist = purrr::pmap(list(output, !!rlang::sym(rows), !!rlang::sym(cols)), function(out, par_value1, par_value2) {
      	d <- make_kernel_dist(out, var_names, lims, kde_fun, n, h, adjust)
      	df <- data.frame(x = d$x, d = d$d, U = pmin(-log(d$d), Umax))
        df$rows <- par_value1
        df$cols <- par_value2
        df
      }))
  }
  if (individual_landscape) {
    df_nested <- df_nested %>%
    	dplyr::rowwise() %>%
    	dplyr::mutate(
    		l_list = list(purrr::quietly(make_2d_static)(output, x, lims, kde_fun, n, h, adjust = 1, Umax)$result)
    	) %>%
    	dplyr::ungroup()
  }
  df_nested$output <- NULL
  df_all <- do.call(rbind, df_nested$dist)

  message("Making the plots...")
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

  result <- list(dist_raw = df_nested, dist = df_all, plot = p, x = x, rows = rows, cols = cols, lims = lims, kde_fun = kde_fun, n = n, h = h, adjust = adjust, Umax = Umax, individual_landscape = individual_landscape)
  class(result) <- c("2d_matrix_landscape", "2d_landscape_batch", "landscape")
  return(result)
}

#' Make a matrix of 3D static landscape plots for one or two parameters
#'
#' Currently only 3D (x, y, color) is supported. Matrices with 3D (x, y, z) plots are not supported.
#'
#' @inheritParams make_3d_static
#' @inheritParams make_2d_matrix
#'
#' @return A `3d_matrix_landscape` object that describes the landscape of the system, including the smoothed distribution and the landscape plot.
#'
#' @export
make_3d_matrix <- function(bs, x, y, rows = NULL, cols, lims, kde_fun = c("ks", "MASS"), n = 200, h, adjust = 1, Umax = 5, individual_landscape = TRUE) {
	kde_fun <- kde_fun[1]
	var_names <- c(x, y)
	h <- determine_h_batch(bs, var_names, kde_fun, h %>% rlang::maybe_missing(), adjust)
	lims <- determine_lims_batch(bs, var_names, lims %>% rlang::maybe_missing())
	message("Wrangling the data...")
  if (is.null(rows)) {
    df_nested <- bs %>%
      dplyr::mutate(dist = purrr::pmap(list(output, bs[, cols]), function(out, par_value1) {
        d <- make_kernel_dist(out, var_names, lims, kde_fun, n, h, adjust)
        df <- make_2d_tidy_dist(d)
        df$cols <- par_value1
        df
      }))
  } else {
    df_nested <- bs %>%
      dplyr::mutate(dist = purrr::pmap(list(output, bs[, rows], bs[, cols]), function(out, par_value1, par_value2) {
        d <- make_kernel_dist(out, var_names, lims, kde_fun, n, h, adjust)
        df <- make_2d_tidy_dist(d)
        df$rows <- par_value1
        df$cols <- par_value2
        df
      }))
  }

  if (individual_landscape) {
    df_nested <- df_nested %>%
      dplyr::rowwise() %>%
    	dplyr::mutate(
    		l_list = list(purrr::quietly(make_3d_static)(output, x, y, lims, kde_fun, n, h, adjust = 1, Umax)$result)
    	) %>%
      dplyr::ungroup()
  }
  df_nested$output <- NULL
  df_all <- do.call(rbind, df_nested$dist)

  message("Making the plots...")
  rows_labeller <- function(x) paste0(rows, ": ", x)
  cols_labeller <- function(x) paste0(cols, ": ", x)
  p <- ggplot2::ggplot(data = df_all, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = pmin(-log(d), Umax))) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x = x, y = y, fill = "U") +
    ggplot2::theme_bw()

  if (is.null(rows)) {
    p <- p + ggplot2::facet_wrap(. ~ cols, labeller = ggplot2::labeller(.cols = ggplot2::as_labeller(cols_labeller)))
  } else {
    p <- p + ggplot2::facet_grid(rows ~ cols, labeller = ggplot2::labeller(.rows = ggplot2::as_labeller(rows_labeller), .cols = ggplot2::as_labeller(cols_labeller)))
  }

  result <- list(dist_raw = df_nested, dist = df_all, plot = p, x = x, y = y, rows = rows, cols = cols, Umax = Umax, n = n, lims = lims, h = h, kde_fun = kde_fun)
  class(result) <- c("3d_matrix_landscape", "3d_landscape_batch", "landscape")
  return(result)
}

#' Make 3d animations from multiple simulations
#'
#' @param fr The names of the parameters used to represent frames in the animation.
#' @param Umax The maximum displayed value of potential.
#' @inheritParams make_3d_static
#' @inheritParams make_2d_matrix
#' @param mat_3d Also make the matrix by [make_3d_matrix()]? If so, the matrix can be drawn with `plot(<landscape>, 3)`.
#'
#' @return A `3d_animation_landscape` object that describes the landscape of the system, including the smoothed distribution and the landscape plot.
#'
#' @export
make_3d_animation <- function(bs, x, y, fr, Umax = 5, n = 200, lims = c(-0.1, 1.1, -0.1, 1.1), h = 1e-3, kde_fun = "ks", individual_landscape = TRUE, mat_3d = FALSE) {
	kde_fun <- kde_fun[1]
	var_names <- c(x, y)
	h <- determine_h_batch(bs, var_names, kde_fun, h %>% rlang::maybe_missing(), adjust)
	lims <- determine_lims_batch(bs, var_names, lims %>% rlang::maybe_missing())
	message("Wrangling the data...")
	df_nested <- bs %>%
		dplyr::rowwise() %>%
		dplyr::mutate(
			dist = list(make_kernel_dist(output, var_names, lims, kde_fun, n, h, adjust))
		)

	if (individual_landscape) {
		df_nested <- df_nested %>%
			dplyr::mutate(
				l_list = list(purrr::quietly(make_3d_static)(output, x, y, lims, kde_fun, n, h, adjust = 1, Umax)$result)
			)
	}

	df_nested$output <- NULL

	df_nested_tidy <- df_nested %>%
		dplyr::mutate(tidy_dist = list(make_2d_tidy_dist(dist, !!rlang::sym(fr), var_name = "fr"))) %>%
		dplyr::ungroup()

	df_nested_collect <- do.call(rbind, df_nested_tidy$tidy_dist)
	message("Done!")

	message("Making the plots...")
	p <-
		df_nested_collect %>%
		plotly::plot_ly(x = ~x, y = ~y, z = pmin(-log(.$d %>% t()), Umax), color = pmin(-log(.$d %>% t()), Umax), frame = ~fr) %>%
		plotly::add_markers(size = I(5)) %>%
		plotly::layout(scene = list(xaxis = list(title = x), yaxis = list(title = y), zaxis = list(title = "U"))) %>%
		plotly::colorbar(title = "U") %>%
		plotly::animation_slider(
			currentvalue = list(prefix = paste0(fr, ": "))
		)

	p2 <- ggplot2::ggplot(df_nested_collect, ggplot2::aes(x = x, y = y)) +
		ggplot2::geom_raster(ggplot2::aes(fill = pmin(-log(d), Umax))) +
		ggplot2::scale_fill_viridis_c() +
		ggplot2::labs(x = x, y = y, fill = "U") +
		ggplot2::theme_bw() +
		gganimate::transition_states(df_nested_collect$fr) +
		ggplot2::labs(subtitle = paste0(fr, ": {closest_state}"))

	if (mat_3d) {
		mat_3d <- make_3d_matrix(bs = bs, x = x, y = y, rows = NULL, cols = fr, lims, kde_fun, n, h, adjust, Umax, individual_landscape)
	}

	result <- list(dist_raw = df_nested, dist = df_nested_collect, plot = p, plot_2 = p2, mat_3d = mat_3d, x = x, y = y, fr = fr, Umax = Umax, n = n, lims = lims, h = h, kde_fun = kde_fun)
	class(result) <- c("3d_animation_landscape", "3d_landscape_batch", "landscape")
	return(result)
}
