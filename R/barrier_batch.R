#' @rdname calculate_barrier
#' @export
calculate_barrier.3d_animation_landscape <- function(l, ...) {
  calculate_barrier_3d_batch(l, ...)
}

#' @rdname calculate_barrier
#' @export
calculate_barrier.3d_matrix_landscape <- function(l, ...) {
  calculate_barrier_3d_batch(l, ...)
}

#' @rdname calculate_barrier
#' @export
calculate_barrier.2d_matrix_landscape <- function(l, ...) {
  calculate_barrier_2d_batch(l, ...)
}

#' Make a grid for calculating barriers for 2d landscapes
#'
#' @param vg A \code{var_grid} object.
#' @param start_location_value,start_r,end_location_value,end_r Default values for finding local minimum. See \code{\link{calculate_barrier_3d_batch}}.
#' @param df A data frame for the variables. Use \code{print_template = TRUE} to get a template.
#' @param print_template Print a template for \code{df}.
#'
#' @export
make_barrier_grid_2d <- function(vg, start_location_value = 0, start_r = 0.1, end_location_value = 0.7, end_r = 0.15, df = NULL, print_template = FALSE) {
  if (!"var_grid" %in% class(vg)) stop("`vg` should be a var_grid object")
  result <- vg

  if (is.null(df)) {
    result <- result %>% dplyr::mutate(
      start_location_value = start_location_value,
      start_r = start_r,
      end_location_value = end_location_value,
      end_r = end_r
    )
  } else {
    result <- cbind(result, df %>% dplyr::select((ncol(.) - 3):ncol(.)))
  }

  if (print_template) {
    dput(result %>% dplyr::select((ncol(.) - 3):ncol(.)))
  }

  class(result) <- c("barrier_grid_2d", "data.frame")
  return(result)
}

#' Calculate barrier from a 2D landscape with multiple simulations
#'
#' @param l A \code{2d_animation_landscape} (not implemented yet) or a \code{2d_matrix_landscape}.
#' @param bg A \code{barrier_grid_3d} object if you want to use different parameters for each condition. Otherwise \code{NULL}.
#' @param start_location_value,end_location_value The initial position (in value) for searching the start/end point.
#' @param start_r,end_r The searching (L1) radius for searching the start/end point.
#' @param base The base of the log function.
#'
#' @export
calculate_barrier_2d_batch <- function(l, bg = NULL, start_location_value = 0, start_r = 0.1, end_location_value = 0.7, end_r = 0.15, base = exp(1)) {
  if (!"2d_matrix_landscape" %in% class(l)) {
    stop("l should be `2d_matrix_landscape` object.")
  }
  d <- l$dist_raw
  if (!"l_list" %in% colnames(d)) {
    stop("l must contain a list of individual landscapes. Use individual_landscape = TRUE in `make_2d_matrix")
  }

  if (is.null(bg)) {
    d <- d %>%
      dplyr::rowwise() %>%
      dplyr::mutate(b = list(calculate_barrier_2d(l_list, start_location_value, start_r, end_location_value, end_r, base)))
  } else {
    if (!"barrier_grid_2d" %in% class(bg)) stop("`bg` should be a `barrier_grid_2d`.")
    d <- d %>%
      dplyr::ungroup() %>%
      dplyr::left_join(bg %>% dplyr::select(1, (ncol(.) - 3):ncol(.)), by = "var_list") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(b = list(calculate_barrier_2d(l_list, start_location_value, start_r, end_location_value, end_r, base)))
  }

  d <- d %>%
    dplyr::mutate(
      start_x = b$local_min_start$location["x_value"],
      start_U = b$local_min_start$U,
      end_x = b$local_min_end$location["x_value"],
      end_U = b$local_min_end$U,
      saddle_x = b$saddle_point$location["x_value"],
      saddle_U = b$saddle_point$U
    ) %>%
    dplyr::ungroup()

  point_all <- d %>% dplyr::select(start_x:saddle_U)

  if ("2d_matrix_landscape" %in% class(l)) {
    point_all[, "cols"] <- d[, l$cols]
    if (!is.null(l$rows)) {
      point_all[, "rows"] <- d[, l$rows]
    }
  }

  p <- ggplot2::ggplot() +
    ggplot2::labs(x = l$x, y = l$y) +
    ggplot2::geom_point(data = point_all, ggplot2::aes(x = start_x, y = start_U)) +
    ggplot2::geom_point(data = point_all, ggplot2::aes(x = end_x, y = end_U)) +
    ggplot2::geom_point(data = point_all, ggplot2::aes(x = saddle_x, y = saddle_U), color = "red")

  geom <- list(
    ggplot2::geom_point(data = point_all, ggplot2::aes(x = start_x, y = start_U), color = "white"),
    ggplot2::geom_point(data = point_all, ggplot2::aes(x = end_x, y = end_U), color = "white"),
    ggplot2::geom_point(data = point_all, ggplot2::aes(x = saddle_x, y = saddle_U), color = "red")
  )
  rows_labeller <- function(x) paste0(l$rows, ": ", x)
  cols_labeller <- function(x) paste0(ifelse(is.null(l$cols), l$fr, l$cols), ": ", x)
  if ("rows" %in% colnames(point_all)) {
    p <- p + ggplot2::facet_grid(rows ~ cols, labeller = ggplot2::labeller(.rows = ggplot2::as_labeller(rows_labeller), .cols = ggplot2::as_labeller(cols_labeller)))
  } else {
    p <- p + ggplot2::facet_wrap(. ~ cols, labeller = ggplot2::labeller(.cols = ggplot2::as_labeller(cols_labeller)))
  }

  result <- list(
    dist_raw = d,
    point_all = point_all,
    plot = p,
    geom = geom,
    x = l$x, rows = l$rows, cols = ifelse(is.null(l$cols), l$fr, l$cols)
  )

  class(result) <- c("barrier_2d_batch", "barrier")

  return(result)
}


#' Make a grid for calculating barriers for 3d landscapes
#'
#' @param vg A \code{var_grid} object.
#' @param start_location_value,start_r,end_location_value,end_r Default values for finding local minimum. See \code{\link{calculate_barrier_3d_batch}}.
#' @param df A data frame for the variables. Use \code{print_template = TRUE} to get a template.
#' @param print_template Print a template for \code{df}.
#'
#' @export
make_barrier_grid_3d <- function(vg, start_location_value = c(0, 0), start_r = 0.1, end_location_value = c(0.7, 0.6), end_r = 0.15, df = NULL, print_template = FALSE) {
  if (!"var_grid" %in% class(vg)) stop("`vg` should be a var_grid object")
  result <- vg

  if (length(start_r) == 1) start_r <- rep(start_r, 2)
  if (length(end_r) == 1) end_r <- rep(end_r, 2)

  if (is.null(df)) {
    result <- result %>% dplyr::mutate(
      start_location_value = list(start_location_value),
      start_r = list(start_r),
      end_location_value = list(end_location_value),
      end_r = list(end_r)
    )
  } else {
    result <- cbind(result, df %>% dplyr::select((ncol(.) - 3):ncol(.)))
  }

  if (print_template) {
    dput(result %>% dplyr::select((ncol(.) - 3):ncol(.)))
  }

  class(result) <- c("barrier_grid_3d", "data.frame")
  return(result)
}

#' Calculate barrier from a 3D landscape with multiple simulations
#'
#' @param l A \code{3d_animation_landscape} or a \code{3d_matrix_landscape}.
#' @param bg A \code{barrier_grid_3d} object if you want to use different parameters for each condition. Otherwise \code{NULL}.
#' @param start_location_value,end_location_value The initial position (in value) for searching the start/end point.
#' @param start_r,end_r The searching (L1) radius for searching the start/end point.
#' @param Umax The highest possible value of the potential function.
#' @param expand If the values in the range all equal to \code{Umax}, expand the range or not?
#' @param omit_unstable If a state is not stable (the "local minimum" overlaps with the saddle point), omit that state or not?
#' @param base The base of the log function.
#'
#' @export
calculate_barrier_3d_batch <- function(l, bg = NULL, start_location_value = c(0, 0), start_r = 0.1, end_location_value = c(0.7, 0.6), end_r = 0.15, Umax, expand = TRUE, omit_unstable = FALSE, base = exp(1)) {
  if (!any(c("3d_animation_landscape", "3d_matrix_landscape") %in% class(l))) {
    stop("l should be `3d_animation_multichain_landscape` or `3d_matrix_landscape` object.")
  }
  d <- l$dist_raw
  if (!"l_list" %in% colnames(d)) {
    stop("l must contain a list of individual landscapes. Use individual_landscape = TRUE in `make_3d_animation` or `make_3d_matrix")
  }
  if (missing(Umax)) Umax <- l$Umax

  if (is.null(bg)) {
    d <- d %>%
      dplyr::rowwise() %>%
      dplyr::mutate(b = list(calculate_barrier_3d(l_list, start_location_value, start_r, end_location_value, end_r, Umax, expand, omit_unstable, base)))
  } else {
    if (!"barrier_grid_3d" %in% class(bg)) stop("`bg` should be a `barrier_grid_3d`.")
    d <- d %>%
      dplyr::ungroup() %>%
      dplyr::left_join(bg %>% dplyr::select(1, (ncol(.) - 3):ncol(.)), by = "var_list") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(b = list(calculate_barrier_3d(l_list, start_location_value, start_r, end_location_value, end_r, Umax, expand, omit_unstable, base)))
  }

  d <- d %>%
    dplyr::mutate(
      start_x = b$local_min_start$location["x_value"],
      start_y = b$local_min_start$location["y_value"],
      start_U = b$local_min_start$U,
      end_x = b$local_min_end$location["x_value"],
      end_y = b$local_min_end$location["y_value"],
      end_U = b$local_min_end$U,
      saddle_x = b$saddle_point$location["x_value"],
      saddle_y = b$saddle_point$location["y_value"],
      saddle_U = b$saddle_point$U
    ) %>%
    dplyr::ungroup()

  point_all <- d %>% dplyr::select(start_x:saddle_U)

  if ("3d_animation_landscape" %in% class(l)) {
    d <- d %>%
      dplyr::rowwise() %>%
      dplyr::mutate(min_path_var = list(b$min_path %>% dplyr::mutate(cols = !!rlang::sym(l$fr)))) %>%
      dplyr::ungroup()
    point_all[, "cols"] <- d[, l$fr]
  }
  if ("3d_matrix_landscape" %in% class(l)) {
    d <- d %>%
      dplyr::rowwise() %>%
      dplyr::mutate(min_path_var = list(b$min_path %>% dplyr::mutate(cols = !!rlang::sym(l$cols)))) %>%
      dplyr::ungroup()
    point_all[, "cols"] <- d[, l$cols]
    if (!is.null(l$rows)) {
      d <- d %>%
        dplyr::rowwise() %>%
        dplyr::mutate(min_path_var = list(min_path_var %>% dplyr::mutate(rows = !!rlang::sym(l$rows)))) %>%
        dplyr::ungroup()
      point_all[, "rows"] <- d[, l$rows]
    }
  }

  min_path_all <- do.call(rbind, d$min_path_var)


  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = min_path_all, mapping = ggplot2::aes(x = x_value, y = y_value)) +
    ggplot2::labs(x = l$x, y = l$y)

  geom <- list(
    ggplot2::geom_path(data = min_path_all, mapping = ggplot2::aes(x = x_value, y = y_value), color = "white"),
    ggplot2::geom_point(data = point_all, ggplot2::aes(x = start_x, y = start_y), color = "white"),
    ggplot2::geom_point(data = point_all, ggplot2::aes(x = end_x, y = end_y), color = "white"),
    ggplot2::geom_point(data = point_all, ggplot2::aes(x = saddle_x, y = saddle_y), color = "red")
  )
  rows_labeller <- function(x) paste0(l$rows, ": ", x)
  cols_labeller <- function(x) paste0(ifelse(is.null(l$cols), l$fr, l$cols), ": ", x)
  if ("rows" %in% colnames(min_path_all)) {
    p <- p + ggplot2::facet_grid(rows ~ cols, labeller = ggplot2::labeller(.rows = ggplot2::as_labeller(rows_labeller), .cols = ggplot2::as_labeller(cols_labeller)))
  } else {
    p <- p + ggplot2::facet_wrap(. ~ cols, labeller = ggplot2::labeller(.cols = ggplot2::as_labeller(cols_labeller)))
  }

  result <- list(
    dist_raw = d,
    min_path_all = min_path_all,
    point_all = point_all,
    plot = p,
    geom = geom,
    x = l$x, y = l$y, rows = l$rows, cols = ifelse(is.null(l$cols), l$fr, l$cols)
  )

  class(result) <- c("barrier_3d_batch", "barrier")

  return(result)
}
