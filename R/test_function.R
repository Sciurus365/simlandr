#' A simple simulation function for testing
#'
#' @param par1,par2 Two parameters. \code{par1} contains \code{var1};
#' \code{par2} contains \code{var2} and \code{var3}.
#' @param length The length of simulation.
#'
#' @return A matrix of simulation results.
#'
#' @seealso \code{\link{sim_fun_test2}} for a more realistic example. \code{\link{batch_simulation}} for a concrete example.
#'
#' @export
#'
sim_fun_test <- function(par1, par2, length = 1000) {
	output <- matrix(nrow = length, ncol = 3)
	colnames(output) <- c("out1", "out2", "out3")
	output[1, ] <- c(par1$var1, par2$var2, rnorm(1, sd = 0.01))
	for (i in 2:length) {
		output[i, 1] <- 0.5 * output[i - 1, 1] + output[i - 1, 2] + par2$var3 + par1$var1 * par2$var2
		output[i, 2] <- -0.5 * output[i - 1, 1] + output[i - 1, 2] + par2$var3
		output[i, 3] <- output[i - 1, 3] + rnorm(1, sd = 0.01)
	}
	return(output)
}


#' A simple yet meaningful simulation function for testing
#'
#' This is a toy system with bi-stability. You can use it to try out functions.
#' Model specification:
#' \deqn{dx/dt = cy - y^3 + \sigma dW/dt}
#' \deqn{y = x + \sigma dW}
#'
#' @param variable,parameter Two sets of parameters. \code{variable} contains \code{x} and \code{y};
#' \code{parameter} contains \code{c}, which controls the shape of the landscape, and \code{sigmasq}, which controls the amplitude of noise.
#' @param length The length of simulation.
#'
#' @return A matrix of simulation results.
#'
#' @seealso \code{\link{sim_fun_test}} and \code{\link{batch_simulation}}.
#'
#' @export
#'
sim_fun_test2 <- function(variable = list(x = 1, y = 1), parameter = list(sigmasq = 25, c = 1), length = 1e5) {
	output <- matrix(nrow = length, ncol = 2)
	colnames(output) <- c("x", "y")
	output[1, ] <- c(variable$x, variable$y)
	for (i in 2:length) {
		## Simplest chemical https://bmcsystbiol.biomedcentral.com/articles/10.1186/1752-0509-3-90
		# output[i, "x"] <- max(0, 2 * output[i - 1, "y"] - output[i - 1, "x"]^2 - output[i - 1, "x"] * output[i - 1, "y"] - output[i - 1, "x"] + rnorm(1, sd = parameter$sd))
		# output[i, "y"] <- max(0, output[i - 1, "x"]^2 - output[i - 1, "y"] + rnorm(1, sd = parameter$sd))
		## Lotka–Volterra
		# output[i, "x"] <- max(0, output[i - 1, "x"] + 2/3*output[i - 1, "x"] - 4/3*output[i - 1, "x"] * output[i - 1, "y"] + rnorm(1, sd = parameter$sd))
		# output[i, "y"] <- max(0, output[i - 1, "y"] + output[i - 1, "x"] * output[i - 1, "y"] - output[i - 1, "y"] + rnorm(1, sd = parameter$sd))
		## Simplest single-variable
		output[i, "x"] <- output[i - 1, "x"] + 0.01*(parameter$c * output[i - 1, "y"] - output[i - 1, "y"]^3 + rnorm(1, sd = sqrt(parameter$sigmasq)))
		output[i, "y"] <- output[i - 1, "x"] + 0.01*(rnorm(1, sd = sqrt(parameter$sigmasq)))
	}
	return(output)
}
