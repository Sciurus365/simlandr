#' A simple simulation function for testing
#'
#' @param par1,par2 Two parameters. `par1` contains `var1`;
#' `par2` contains `var2` and `var3`.
#' @param length The length of simulation.
#'
#' @return A matrix of simulation results.
#'
#' @seealso [sim_fun_test2()] for a more realistic example. [batch_simulation()] for a concrete example.
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
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `sim_fun_test2()` was renamed to `sim_fun_grad()` to create a more
#' consistent API.
#'
#' @param ... Parameters to be passed to [sim_fun_grad()]
#' @keywords internal
#' @export
sim_fun_test2 <- function(...) {
  lifecycle::deprecate_warn("0.1.3", "sim_fun_test2()", "sim_fun_grad()")
  sim_fun_grad(...)
}


#' A simple gradient simulation function for testing
#'
#' This is a toy stochastic gradient system which can have bistability in some conditions.
#' Model specification:
#' \deqn{U = x^4 + y^4 + axy + bx + cy}
#' \deqn{dx/dt = - \partial U/ \partial x + \sigma dW/dt = - 4x^3 - ay - b + \sigma dW/dt}
#' \deqn{dy/dt = - \partial U/ \partial y + \sigma dW/dt = - 4y^3 - ax - c + \sigma dW/dt}
#'
#' @param initial,parameter Two sets of parameters. `initial` contains the initial value of `x` and `y`;
#' `parameter` contains `a,b,c`, which control the shape of the potential landscape,
#' and `sigmasq`, which is the square of \eqn{\sigma} and controls the amplitude of noise.
#' @param length The length of simulation.
#' @param stepsize The step size used in the Euler method.
#' @param seed The initial seed that will be passed to `set.seed()` function.
#'
#' @return A matrix of simulation results.
#'
#' @seealso [sim_fun_nongrad()] and [batch_simulation()].
#'
#' @export
#'
sim_fun_grad <- function(initial = list(x = 0, y = 0), parameter = list(a = -4, b = 0, c = 0, sigmasq = 1), length = 1e5, stepsize = 0.01, seed = NULL) {
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

#' A simple non-gradient simulation function for testing
#'
#' This is a toy stochastic non-gradient system which can have multistability in some conditions.
#' Model specification:
#'
#'
#' \deqn{\frac {dx_ {1}}{dt}  =  \frac {ax_ {1}^ {n}}{S^ {n}+x_ {1}^ {n}} + \frac {bS^ {n}}{S^ {n}+x_ {2}^ {n}} - kx_ {1}+ \sigma_1 dW_1/dt}
#' \deqn{\frac {dx_ {2}}{dt}  =  \frac {ax_ {2}^ {n}}{S^ {n}+x_ {2}^ {n}} + \frac {bS^ {n}}{S^ {n}+x_ {1}^ {n}} - kx_ {2}+ \sigma_2 dW_2/dt}
#' \deqn{\frac {da}{dt} = -\lambda a+ \sigma_3 dW_3/dt}
#'
#' @param initial,parameter Two sets of parameters. `initial` contains the initial value of `x1`, `x2`, and `a`;
#' `parameter` contains `b,k,S,n,lambda`, which control the model dynamics,
#' and `sigmasq1,sigmasq2,sigmasq3`, which are the squares of \eqn{\sigma_1,\sigma_2,\sigma_3} and controls the amplitude of noise.
#' @param constrain_a Should the value of \code{a} be constrained? (\code{TRUE} by default).
#' @param amin,amax If \code{constrain_a}, the minimum and maximum values of a.
#' @param length The length of simulation.
#' @param stepsize The step size used in the Euler method.
#' @param seed The initial seed that will be passed to `set.seed()` function.
#' @param progress Show progress bar of the simulation?
#'
#' @return A matrix of simulation results.
#'
#' @seealso [sim_fun_grad()] and [batch_simulation()].
#'
#' @references
#' Wang, J., Zhang, K., Xu, L., & Wang, E. (2011). Quantifying the Waddington landscape and biological paths for development and differentiation. Proceedings of the National Academy of Sciences, 108(20), 8257-8262.
#' \doi{10.1073/pnas.1017017108}
#'
#' @export
#'
sim_fun_nongrad <- function(initial = list(x1 = 0, x2 = 0, a = 1),
                            parameter = list(
                              b = 1, k = 1, S = 0.5, n = 4,
                              lambda = 0.01,
                              sigmasq1 = 8, sigmasq2 = 8, sigmasq3 = 2
                            ), constrain_a = TRUE, amin = -0.3, amax = 1.8,
                            length = 1e5, stepsize = 0.01, seed = NULL, progress = TRUE) {
  set.seed(seed)
  output <- matrix(nrow = length, ncol = 4)
  colnames(output) <- c("x1", "x2", "a", "delta_x")
  output[1, ] <- c(initial$x1, initial$x2, initial$a, initial$x1 - initial$x2)
  if (progress) {
    pb <- progress::progress_bar$new(total = length)
    pb$tick()
  }
  for (i in 2:length) {
    x1_temp <- output[i - 1, "x1"]
    x2_temp <- output[i - 1, "x2"]
    a_temp <- output[i - 1, "a"]
    for (j in 1:(1 / stepsize)) {
      x1_temp_new <- x1_temp +
        stepsize * (
          a_temp / ((parameter$S / x1_temp)^parameter$n + 1) +
            parameter$b / (1 + (x2_temp / parameter$S)^parameter$n) -
            parameter$k * x1_temp +
            rnorm(1, sd = sqrt(parameter$sigmasq1 * stepsize))
        )
      x2_temp_new <- x2_temp +
        stepsize * (
          a_temp / ((parameter$S / x2_temp)^parameter$n + 1) +
            parameter$b / (1 + (x1_temp / parameter$S)^parameter$n) -
            parameter$k * x2_temp +
            rnorm(1, sd = sqrt(parameter$sigmasq2 * stepsize))
        )
      a_temp_new <- a_temp + stepsize * (-parameter$lambda * a_temp + rnorm(1, sd = sqrt(parameter$sigmasq3 * stepsize)))
      if (constrain_a) {
        a_temp_new <- min(max(a_temp_new, amin), amax)
      }

      x1_temp <- x1_temp_new
      x2_temp <- x2_temp_new
      a_temp <- a_temp_new
    }
    output[i, "x1"] <- x1_temp
    output[i, "x2"] <- x2_temp
    output[i, "a"] <- a_temp
    output[i, "delta_x"] <- x1_temp - x2_temp
    if (progress) pb$tick()
  }
  return(output)
}
