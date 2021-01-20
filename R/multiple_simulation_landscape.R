#' Make a tidy data frame from smooth 2d distribution matrix
#'
#' @param dist_2d \code{kde2d} distribution.
#' @param value The value of the variable of interest.
#' @param var_name The name of the variable.
#'
#' @return A tidy data frame.
#'
#' @export
#'
make_tidy_dist <- function(dist_2d, value, var_name) {
  df <- cbind(
    expand.grid(x = dist_2d$x, y = dist_2d$y),
    expand.grid(
      x_index = 1:length(dist_2d$x),
      y_index = 1:length(dist_2d$y)
    )
  )
  z_mat <- dist_2d$z
  df <- df %>%
    dplyr::mutate(z = purrr::map2_dbl(x_index, y_index, function(x, y, zm) {
      zm[x, y]
    }, zm = z_mat))
  df[, var_name] <- value
  return(df)
}

#' Make 3d animations from multiple simulations
#'
#' @param bs A \code{batch_simulation} object created by \code{\link{batch_simulation}.}
#' @param x,y,fr The names of the target variables.
#' \code{fr} corresponds to the \code{frame} parameter in plotly.
#' @param zmax The maximum displayed value of potential.
#' @param individual_plot Make individual plot for each var value?
#'
#' @export
make_3d_animation_multisim <- function(bs, x, y, fr, zmax = 5, individual_plot = FALSE) {
  message("Wrangling data...")
  df_multichannel <- bs %>%
    dplyr::mutate(
      dist = purrr::map(output, make_kernel_dist, x = x, y = y)
    )

  if (individual_plot) {
    df_multichannel <- df_multichannel %>%
      dplyr::mutate(
        plot = purrr::map(output, make_3d_static, x = x, y = y)
      )
  }

  df_multichannel_tidy <- df_multichannel %>%
    dplyr::mutate(tidy_dist = purrr::map2(dist, df_multichannel[,fr], make_tidy_dist, var_name = fr))

  df_multichannel_collect <- do.call(rbind, df_multichannel_tidy$tidy_dist)
  message("Done!")

  message("Making the plot...")
  p <-
    df_multichannel_collect %>%
    plotly::plot_ly(x = ~x, y = ~y, z = pmin(-log10(.$z), zmax), color = pmin(-log10(.$z), zmax), frame = .[, fr]) %>%
    plotly::add_markers(size = I(5)) %>%
    plotly::layout(scene = list(xaxis = list(title = x), yaxis = list(title = y), zaxis = list(title = "U")))
  message("Done!")

  result <- list(df = df_multichannel, df_collect = df_multichannel_collect, plot = p, x = x, y = y, fr = fr)
  class(result) <- c("3d_animation_multichain_landscape", "landscape")
  return(result)
}

#' Make a matrix of 3d graphs for two parameters
#' @param bs A \code{batch_simulation} object created by \code{\link{batch_simulation}.}
#' @param x,rows,cols The names of the target variables.
#' @param adjust,from,to Passed to \code{density}.
#' @param zmax The maximum displayed value of potential.
#'
#' @export
make_2d_matrix <- function(bs, x, rows, cols, adjust = 50, from = -0.1, to = 1, zmax = 5){
	df_multichannel <- bs %>%
		dplyr::mutate(output2 = purrr::pmap(list(output, bs[,rows], bs[,cols]), function(out, sample_var1, sample_var2){
			d <- stats::density(out[,x], adjust = adjust, from = from, to = to)
			df <- data.frame(x = d$x, y = d$y, U = pmin(-log10(d$y), zmax)) %>%
				dplyr::mutate(rows = sample_var1, cols = sample_var2)
			df
		}))

	df_all <- do.call(rbind, df_multichannel$output2)

	message("Making the plot...")
	p <- df_all %>%
		ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = U)) +
		ggplot2::geom_line() +
		ggplot2::facet_grid(rows ~ cols) +
		ggplot2::theme_bw() + ggplot2::xlab(x)
	message("Done!")

	result <- list(dist1 = df_multichannel, dist2 = df_all, plot = p, x = x, rows = rows, cols = cols)
	class(result) <- c("2d_matrix_landscape", "landscape")
	return(result)
}
