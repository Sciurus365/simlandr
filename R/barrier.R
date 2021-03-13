#' General function for calculating energy barrier
#'
#' @param l A {landscape} or related project.
#' @param ... Other parameters.
#'
#' @seealso \code{\link{calculate_barrier_2d}}, \code{\link{calculate_barrier_3d}}, \code{\link{calculate_barrier_3d_batch}}
#' @export
calculate_barrier <- function(l, ...) {
  UseMethod("calculate_barrier", l)
}

#' @rdname calculate_barrier
#' @export
calculate_barrier.2d_density_landscape <- function(l, ...) {
  calculate_barrier_2d(l, ...)
}

#' @rdname calculate_barrier
#' @export
calculate_barrier.density <- function(l, ...) {
  calculate_barrier_2d(l, ...)
}

#' @rdname calculate_barrier
#' @export
calculate_barrier.2d_density_landscape <- function(l, ...) {
  calculate_barrier_2d(l, ...)
}

#' @rdname calculate_barrier
#' @export
calculate_barrier.3d_static_landscape <- function(l, ...) {
  calculate_barrier_3d(l, ...)
}

#' @rdname calculate_barrier
#' @export
calculate_barrier.list <- function(l, ...) {
  if (!all(c("x", "y", "z") %in% names(l))) stop("Only the lists created by `MASS::kde2d` is supported.")
  calculate_barrier_3d(l, ...)
}

#' Find local minimum of a 2d distribution
#'
#' @param dist An \code{density} distribution object.
#' @param localmin Starting value of finding local minimum.
#' @param r Searching radius.
#'
#' @export
find_local_min_2d <- function(dist, localmin, r) {
  if (!"density" %in% class(dist)) stop("Wrong input. `dist` should be a `density` object.")
  x1 <- localmin[1]
  effective_dist <- dist$y[dist$x > x1 - r & dist$x < x1 + r]
  max_dist <- max(effective_dist)
  min_U <- -log(max_dist)
  location_index <- which(dist$y == max_dist, arr.ind = TRUE)
  location_value <- dist$x[location_index]
  location <- c(location_index, location_value)
  names(location) <- c("x_index", "x_value")
  return(list(U = min_U, location = location))
}

#' Calculate barrier from a 2D landscape
#'
#' @param l A \code{2d_density_landscape} object (recommended) or a \code{density} distribution.
#' @param start_location_value,end_location_value The initial position (in value) for searching the start/end point.
#' @param start_r,end_r The searching radius for searching the start/end point.
#' @param base The base of the log function.
#'
#' @export
calculate_barrier_2d <- function(l, start_location_value = 0, start_r = 0.1, end_location_value = 0.7, end_r = 0.15, base = exp(1)) {
  if ("2d_density_landscape" %in% class(l)) {
    d <- l$dist
  } else {
    d <- l
  }

  local_min_start <- find_local_min_2d(d, start_location_value, start_r)
  local_min_end <- find_local_min_2d(d, end_location_value, end_r)

  s_U <- max(-log(d$y[local_min_start$location["x_index"]:local_min_end$location["x_index"]], base = base))
  s_location_x_index <- which(-log(d$y, base) == s_U)
  s_location <- c(s_location_x_index, d$x[s_location_x_index])
  names(s_location) <- c("x_index", "x_value")

  saddle_point <- list(
    U = s_U,
    location = s_location
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = d$x[s_location_x_index], y = s_U), color = "red") +
    ggplot2::geom_point(mapping = ggplot2::aes(x = local_min_start$location["x_value"], y = local_min_start$U)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = local_min_end$location["x_value"], y = local_min_end$U)) +
    ggplot2::labs(x = ifelse(!is.character(l$x), l$x, "x"), y = "U")
  result <- list(
    local_min_start = local_min_start,
    local_min_end = local_min_end,
    saddle_point = saddle_point,
    delta_U_start = saddle_point$U - local_min_start$U,
    delta_U_end = saddle_point$U - local_min_end$U,
    plot = p,
    geom = list(
      ggplot2::geom_point(mapping = ggplot2::aes(x = d$x[s_location_x_index], y = s_U), color = "red"),
      ggplot2::geom_point(mapping = ggplot2::aes(x = local_min_start$location["x_value"], y = local_min_start$U), color = "white"),
      ggplot2::geom_point(mapping = ggplot2::aes(x = local_min_end$location["x_value"], y = local_min_end$U), color = "white")
    )
  )
  class(result) <- c("barrier_2d", "barrier")
  return(result)
}

#' Find local minimum of a 3d distribution
#'
#' @param dist An \code{kde2d} distribution object.
#' @param localmin Starting value of finding local minimum.
#' @param r Searching (L1) radius.
#' @param zmax The highest possible value of the potential function.
#' @param first_called Is this function first called by another function?
#'
#' @export
find_local_min_3d <- function(dist, localmin, r, zmax, first_called = TRUE) {
  if (!is.matrix(dist$z)) stop("Wrong input. `dist` should be a list with x, y, and z, and z should be a matrix.")
  x1 <- localmin[1]
  y1 <- localmin[2]
  if(length(r) == 1) r <- rep(r, 2)
  effective_dist <- dist$z[
    dist$x > x1 - r[1] & dist$x < x1 + r[1],
    dist$y > y1 - r[2] & dist$y < y1 + r[2]
  ]
  max_dist <- max(effective_dist)
  min_U <- -log(max_dist)

  if(min_U > zmax){
    if(first_called) message("The U in this range is too high. Searching range expanded...")
    return(find_local_min_3d(dist, localmin, c(r[1] + dist$x[2] - dist$x[1], r[2] + dist$y[2] - dist$y[1]), zmax, first_called = FALSE))
  }
  location_index <- which(dist$z == max_dist, arr.ind = TRUE) %>% apply(2, function(x) as.integer(median(x)))
  location_value <- c(dist$x[location_index[1]], dist$y[location_index[2]])
  location <- c(location_index, location_value)
  names(location) <- c("x_index", "y_index", "x_value", "y_value")
  if(!first_called) message(paste0("r = c(", r[1], ",", r[2], ")"))
  return(list(U = min_U, location = location))
}

#' Calculate barrier from a 3D landscape
#'
#' @param l A \code{3d_static_landscape} object (recommended) or a \code{kde2d} distribution.
#' @param start_location_value,end_location_value The initial position (in value) for searching the start/end point.
#' @param start_r,end_r The searching (L1) radius for searching the start/end point.
#' @param zmax The highest possible value of the potential function.
#' @param base The base of the log function.
#'
#' @export
calculate_barrier_3d <- function(l, start_location_value = c(0, 0), start_r = 0.1, end_location_value = c(0.7, 0.6), end_r = 0.15, zmax, base = exp(1)) {
  if ("3d_static_landscape" %in% class(l)) {
    d <- l$dist
  } else {
    d <- l
  }

  if(missing(zmax)) zmax <- l$zmax

  local_min_start <- find_local_min_3d(d, start_location_value, start_r, zmax)
  local_min_end <- find_local_min_3d(d, end_location_value, end_r, zmax)

  reticulate::source_python(file = system.file("python/dijkstra.py", package = "simlandr"))

  min_path_index <- reticulate::py$dijkstra(
    log(d$z, base = base), # result-=1 because python start from 0
    (local_min_start$location[1:2] - 1) %>%
      as.integer() %>%
      {
        reticulate::tuple(.[1], .[2])
      },
    (local_min_end$location[1:2] - 1) %>%
      as.integer() %>% {
        reticulate::tuple(.[1], .[2])
      }
  )

  min_path <- min_path_index %>%
    unlist() %>%
    matrix(ncol = 2, byrow = T) %>%
    as.data.frame() %>%
    {
      colnames(.) <- c("x_index", "y_index")
      . + 1
    } %>%
    dplyr::rowwise() %>%
    dplyr::mutate(x_value = d$x[x_index], y_value = d$y[y_index]) %>%
    dplyr::mutate(U = -log(d$z[x_index, y_index], base = base)) %>%
    dplyr::ungroup()

  s_U <- max(min_path$U)
  s_location_path_index <- as.integer(median(which(min_path$U == s_U)))
  s_location <- c(s_location_path_index, as.numeric(min_path[s_location_path_index[1], 1:4]))
  names(s_location) <- c("path_index", "x_index", "y_index", "x_value", "y_value")

  saddle_point <- list(
    U = s_U,
    location = s_location
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = min_path, mapping = ggplot2::aes(x = x_value, y = y_value)) +
    ggplot2::geom_point(ggplot2::aes(x = local_min_start$location["x_value"], y = local_min_start$location["y_value"]), color = "black") +
    ggplot2::geom_point(ggplot2::aes(x = local_min_end$location["x_value"], y = local_min_end$location["y_value"]), color = "black") +
    ggplot2::geom_point(ggplot2::aes(x = saddle_point$location["x_value"], y = saddle_point$location["y_value"]), color = "red") +
    ggplot2::labs(x = l$x, y = l$y)
  result <- list(
    local_min_start = local_min_start,
    local_min_end = local_min_end,
    saddle_point = saddle_point,
    min_path = min_path,
    delta_U_start = saddle_point$U - local_min_start$U,
    delta_U_end = saddle_point$U - local_min_end$U,
    plot = p,
    geom = list(
      ggplot2::geom_path(data = min_path, mapping = ggplot2::aes(x = x_value, y = y_value), color = "white"),
      ggplot2::geom_point(ggplot2::aes(x = local_min_start$location["x_value"], y = local_min_start$location["y_value"]), color = "white"),
      ggplot2::geom_point(ggplot2::aes(x = local_min_end$location["x_value"], y = local_min_end$location["y_value"]), color = "white"),
      ggplot2::geom_point(ggplot2::aes(x = saddle_point$location["x_value"], y = saddle_point$location["y_value"]), color = "red")
    )
  )
  class(result) <- c("barrier_3d", "barrier")
  return(result)
}


#' @describeIn calculate_barrier_2d Get the barrier height.
#' @param b A \code{barrier_landscape} object.
#'
#' @export
get_barrier_height <- function(b) {
  c(b$delta_U_start, b$delta_U_end)
}

#' @describeIn calculate_barrier Plot a `barrier`
#' @param x A `barrier` object.
#' @param ... Not in use.
plot.barrier <- function(x, ...) {
  x$plot
}

#' Get a ggplot2 geom layer that can be added to a ggplot2 landscape plot
#'
#' This layer can show the saddle point (2d) and the minimal path (3d) on the landscape.
#'
#' @param b A \code{barrier} object.
#'
#' @export
get_geom <- function(b) {
  if (!"barrier" %in% class(b)) stop("b should be a `barrier`")
  return(b$geom)
}
