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
#' This is a toy stochastic gradient system which can have bi-stability in some conditions.
#' Model specification:
#' \deqn{U = x^4 + y^4 + axy + bx + cy}
#' \deqn{dx/dt = - \partial U/ \partial x + \sigma dW/dt = - 4x^3 - ay - b + \sigma dW/dt}
#' \deqn{dy/dt = - \partial U/ \partial y + \sigma dW/dt = - 4y^3 - ax - c + \sigma dW/dt}
#'
#' @param initial,parameter Two sets of parameters. \code{initial} contains the initial value of \code{x} and \code{y};
#' \code{parameter} contains \code{a,b,c}, which control the shape of the potential landscape,
#' and \code{sigmasq}, which is the square of \eqn{\sigma} and controls the amplitude of noise.
#' @param length The length of simulation.
#' @param stepsize The step size used in the Euler method.
#' @param seed The initial seed that will be passed to \code{set.seed()} function.
#'
#' @return A matrix of simulation results.
#'
#' @seealso \code{\link{sim_fun_test}} and \code{\link{batch_simulation}}.
#'
#' @export
#'
sim_fun_test2 <- function(initial = list(x = 0, y = 0), parameter = list(a = -4, b = 0, c = 0, sigmasq = 1), length = 1e5, stepsize = 0.01, seed = NULL) {
  set.seed(seed)
  output <- matrix(nrow = length, ncol = 2)
  colnames(output) <- c("x", "y")
  output[1, ] <- c(initial$x, initial$y)
  for (i in 2:length) {
    x_temp <- output[i - 1, "x"]
    y_temp <- output[i - 1, "y"]
    for (j in 1:(1 / stepsize)) {
      x_temp_new <- x_temp - stepsize * (4 * x_temp^3 + parameter$a * y_temp + parameter$b) + rnorm(1, sd = sqrt(parameter$sigmasq * stepsize))
      y_temp_new <- y_temp - stepsize * (4 * y_temp^3 + parameter$a * x_temp + parameter$c) + rnorm(1, sd = sqrt(parameter$sigmasq * stepsize))
      x_temp <- x_temp_new
      y_temp <- y_temp_new
    }
    output[i, "x"] <- x_temp
    output[i, "y"] <- y_temp
  }
  return(output)
}
