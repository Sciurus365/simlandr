#' Find local minimum of a distribution
#'
#' @param dist An \code{kde2d} distribution object.
#' @param localmin Starting value of finding local minimum
#' @param r Searching radius
#'
#' @export
find_local_min <- function(dist, localmin, r){
	if(!is.matrix(dist$z)) stop("Wrong input. dist should be a list with x, y, and z, and z should be a matrix.")
	x1 <- localmin[1]
	y1 <- localmin[2]
	effective_dist <- dist$z[dist$x > x1-r & dist$x < x1+r,
													 dist$y > y1-r & dist$y < y1+r]
	max_dist <- max(effective_dist)
	min_U <- -log(max_dist)
	location_index <- which(dist$z == max_dist, arr.ind = TRUE)
	location_value <- c(dist$x[location_index[1]], dist$y[location_index[2]])
	location <- c(location_index, location_value)
	names(location) <- c("x_index", "y_index", "x_value", "y_value")
	return(list(U = min_U, location = location))
}

#' Calculate barrier from a 3D landscape
#'
#' @param l A \code{3d_static_landscape} object (recommended) or a \code{kde2d} distribution.
#' @param start_location_value,end_location_value The initial position (in value) for searching the start/end point.
#' @param start_r,end_r The searching radius for searching the start/end point.
#' @param base The base of the log function.
#'
#' @export
calculate_barrier_3d <- function(l, start_location_value = c(0,0), start_r = 0.1, end_location_value = c(0.7,0.6), end_r = 0.15, base = exp(1)){
	if("3d_static_landscape" %in% class(l)) d <- l$dist
	else d <- l

	local_min_start <- find_local_min(d, start_location_value, start_r)
	local_min_end <- find_local_min(d, end_location_value, end_r)

	reticulate::source_python(file = system.file("python/dijkstra.py", package = "simlandr"))

	min_path_index <- reticulate::py$dijkstra(log(d$z, base = base), # result-=1 because python start from 0
																(local_min_start$location[1:2]-1)%>%
																	as.integer %>% {reticulate::tuple(.[1],.[2])},
																(local_min_end$location[1:2]-1)%>%
																	as.integer %>% {reticulate::tuple(.[1],.[2])})

	min_path <- min_path_index %>%
		unlist %>% matrix(ncol = 2, byrow = T) %>%
		as.data.frame() %>%
		{
			colnames(.) <- c("x_index", "y_index")
			.+1
		} %>%
		dplyr::rowwise() %>%
		dplyr::mutate(x_value = d$x[x_index], y_value = d$y[y_index]) %>%
		dplyr::mutate(U = -log(d$z[x_index,y_index], base = base)) %>% dplyr::ungroup()

	s_U <- max(min_path$U)
	s_location_path_index <- which(min_path$U == s_U)
	s_location <- c(s_location_path_index, as.numeric(min_path[s_location_path_index[1], 1:4]))
	names(s_location) <- c("path_index", "x_index", "y_index", "x_value", "y_value")

	saddle_point <- list(
		U = s_U,
		location = s_location
	)

	p <- ggplot2::ggplot() + ggplot2::geom_path(data = min_path, mapping = ggplot2::aes(x = x_value, y = y_value)) +
		ggplot2::labs(x = l$x, y = l$y)
	result <- list(local_min_start = local_min_start,
								 local_min_end = local_min_end,
								 saddle_point = saddle_point,
								 min_path = min_path,
								 delta_U_start = saddle_point$U - local_min_start$U,
								 delta_U_end = saddle_point$U - local_min_end$U,
								 plot = p,
								 geom = ggplot2::geom_path(data = min_path, mapping = ggplot2::aes(x = x_value, y = y_value), color = "white")
	)
	class(result) <- "barrier_landscape"
	return(result)
}

calculate_barrier_3d_batch <- function(l, start_location_value = c(0,0), start_r = 0.1, end_location_value = c(0.7,0.6), end_r = 0.15, base = exp(1)){
	if(!any(c("3d_animation_landscape", "3d_matrix_landscape") %in% class(l))){
		stop("l should be `3d_animation_multichain_landscape` or `3d_matrix_landscape` object.")
	}
	d <- l$dist_raw
	if("l" %in% colnames(d)){
		stop("l must contain individual landscapes. Use individual_landscape = TRUE in `make_3d_animation` or `make_3d_matrix")
	}

	d <- d %>%
		dplyr::rowwise %>%
		dplyr::mutate(b = calculate_barrier_3d(l, start_location_value, start_r, end_location_value, end_r, base)) %>%
		ungroup

	result <- list(dist_raw = d)

	# add plot to the previous one (it's only possible to add to ggplot thing, i.e., mat_3d. So if there's not, raise a warning?)
	# idealy here should be a geom that can be added to the previous plot, as well as a plot with only path but not landscape
	# it's very important to keep api consistant!
	# is there a way to put the parameters in the list?
	# as.list(match.call()) this should work
	class(result) <- "barrier_landscape_batch"
}


#' @describeIn calculate_barrier_2d Get the barrier height.
#' @param b A \code{barrier_landscape} object.
#'
#' @export
get_barrier_height <- function(b){
	c(b$delta_U_start, b$delta_U_end)
}

#' @describeIn calculate_barrier_2d
#' @param x A `barrier_landscape` object.
#' @param ... Not in use.
plot.barrier_landscape <- function(x, ...){
	x$plot
}
