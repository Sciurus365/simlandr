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
	min_U <- -log10(max_dist)
	location_index <- which(dist$z == max_dist, arr.ind = TRUE)
	location_value <- c(dist$x[location_index[1]], dist$y[location_index[2]])
	names(location_value) <- names(location_index)
	return(list(U = min_U, location_index = location_index,
							location_value = location_value))
}




#' Calculate barrier from a 2D distribution
#'
#' @param d A \code{3d_static_landscape} object or a \code{kde2d} distribution.
#' @param start_location,end_location The initial position for searching the start/end point.
#' @param start_r,end_r The searching radius for searching the start/end point.
#' @param base The base of the log function.
#'
#' @export
calculate_barrier_2d <- function(d, start_location = c(0,0), start_r = 0.1, end_location = c(0.7,0.6), end_r = 0.15, base = exp(1)){
	if("3d_static_landscape" %in% class(d)) d <- d$dist

	local_min_start <- find_local_min(d, start_location, start_r)
	local_min_end <- find_local_min(d, end_location, end_r)

	reticulate::source_python(file = system.file("python/dijkstra.py", package = "simlandr"))

	min_path_index <- reticulate::py$dijkstra(d$z, # result-=1 because python start from 0
																(local_min_start$location_index-1)%>%
																	as.integer %>% {reticulate::tuple(.[1],.[2])},
																(local_min_end$location_index-1)%>%
																	as.integer %>% {reticulate::tuple(.[1],.[2])})

	min_path_index <- min_path_index %>%
		unlist %>% matrix(ncol = 2, byrow = T) %>%
		as.data.frame() %>%
		{
			colnames(.) <- c("x", "y")
			.+1
		} %>%
		dplyr::mutate(U = purrr::map2_dbl(x, y, function(x, y) -log(d$z[x,y], base = base)))

	s_U <- max(min_path_index$U)
	s_location_row <- which(min_path_index$U == s_U)
	s_location_index <- as.numeric(min_path_index[s_location_row[1], 1:2])
	names(s_location_index) <- c("x", "y")

	s_location_value <- c(d$x[s_location_index[1]], d$y[s_location_index[2]])

	saddle_point <- list(
		U = s_U,
		location_row = s_location_row,
		location_index = s_location_index,
		location_value = s_location_value
	)

	result <- list(local_min_start = local_min_start,
								 local_min_end = local_min_end,
								 saddle_point = saddle_point,
								 min_path_index = min_path_index,
								 delta_U_start = saddle_point$U - local_min_start$U,
								 delta_U_end = saddle_point$U - local_min_end$U
	)
	class(result) <- "barrier_landscape"
	return(result)
}

#' @describeIn calculate_barrier_2d Get the barrier height.
#' @param b A \code{barrier_landscape} object.
#'
#' @export
get_barrier_height <- function(b){
	c(b$delta_U_start, b$delta_U_end)
}
