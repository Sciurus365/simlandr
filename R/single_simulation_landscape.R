#' Make 2D density-based landscape plot for a single simulation output
#'
#' @param out A matrix of simulation output.
#' @param x The name of the target variable.
#' @param adjust,from,to Passed to \code{density}.
#' @param max_U The maximum displayed value of U.
#'
#' @return A \code{ggplot} graph.
#' @export
make_2d_density <- function(out, x, adjust = 50, from = -0.1, to = 1, max_U = 5){
	d <- density(out[,x], adjust = adjust, from = from, to = to)
	data.frame(x = d$x, y = d$y, U = pmin(-log10(d$y), max_U)) %>%
		ggplot2::ggplot(mapping = aes(x = x, y = U)) +
		ggplot2::geom_line()+
		# geom_smooth(se = F) +
		ggplot2::theme_bw() + ggplot2::xlab(x)
}

#' A function for reversed log transformation
#'
#' @param base The base of logrithm
#'
#' @export
reverselog_trans <- function(base = exp(1)) {
	force(base)
	trans <- function(x) -log(x, base)
	inv <- function(x) base^(-x)
	trans_new(paste0("reverselog-", format(base)), trans, inv,
						log_breaks(base = base),
						domain = c(1e-100, Inf))
}

