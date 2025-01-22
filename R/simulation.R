#' Simulate 1-3D Markovian Stochastic Differential Equations
#'
#' A wrapper to the simulation utilities provided by the \pkg{Sim.DiffProc} package. You may skip this step and write your own simulation function for more customized simulation.
#'
#' @param N The number of time steps.
#' @param M The number of simulations.
#' @param x0 The initial values of the SDE. The number of values determine the dimension of the SDE.
#' @param t0 initial time.
#' @param T terminal time.
#' @param Dt time step. If missing, default will be (T - t0) / N.
#' @param drift An expression of the drift function. The number of expressions determine the dimension of the SDE. Should be the function of `t`, `x`, `y` and `z` (`y` and `z` are only included for 2D or 3D cases).
#' @param diffusion An expression of the diffusion function. The number of expressions determine the dimension of the SDE. Should be the function of `t`, `x`, `y` and `z` (`y` and `z` are only included for 2D or 3D cases).
#' @param corr The correlations between the Brownian motions. Only used for 2D or 3D cases. Must be a real symmetric positive-definite matrix of size 2x2 or 3x3. If NULL, the default is the identity matrix.
#' @param keep_full Whether to keep the full snssde1d/snssde2d/snssde3d object. If TRUE, the full object will be returned. If FALSE, only the simulated values will be returned as a matrix or a list of matrices (when `M >= 2`).
#' @inheritParams Sim.DiffProc::snssde1d
#'
#' @export
#' @return Depending on the value of `keep_full`, the output will be a list of `snssde1d`, `snssde2d` or `snssde3d` objects, or a matrix or a list of matrices of the simulated values.
#'
#' @examples
#' # From the Sim.DiffProc package
#'
#' set.seed(1234, kind = "L'Ecuyer-CMRG")
#' mu <- 4
#' sigma <- 0.1
#' fx <- expression(y, (mu * (1 - x^2) * y - x))
#' gx <- expression(0, 2 * sigma)
#' mod2d <- sim_SDE(drift = fx, diffusion = gx, N = 1000,
#' Dt = 0.01, x0 = c(0, 0), type = "str", method = "rk1",
#' M = 2, keep_full = FALSE)
#'
#' print(as.mcmc.list(mod2d))
sim_SDE <- function(N = 1000, M = 1, x0, t0 = 0, T = 1, Dt = rlang::missing_arg(), drift, diffusion, corr = NULL, alpha = 0.5, mu = 0.5, type = "ito", method = "euler", keep_full = TRUE) {
  if (length(x0) > 3) {
    stop("Only 1-3D SDE is supported.")
  }

  if (length(x0) == 1) {
    sim <- Sim.DiffProc::snssde1d(x0 = x0, t0 = t0, T = T, Dt = Dt, N = N, M = M, drift = drift, diffusion = diffusion, method = method)
  }

  if (length(x0) == 2) {
    sim <- Sim.DiffProc::snssde2d(x0 = x0, t0 = t0, T = T, Dt = Dt, N = N, M = M, drift = drift, diffusion = diffusion, corr = corr, method = method)
  }

  if (length(x0) == 3) {
    sim <- Sim.DiffProc::snssde3d(x0 = x0, t0 = t0, T = T, Dt = Dt, N = N, M = M, drift = drift, diffusion = diffusion, corr = corr, method = method)
  }

  if (keep_full == FALSE) {
    if (length(x0) == 1) {
      if (M == 1) sim <- sim$X[, 1] |> `colnames<-`(c("X"))
      if (M > 1) sim <- lapply(1:M, function(i) sim$X[, i] |> `colnames<-`(c("X")))
    }

    if (length(x0) == 2) {
      if (M == 1) sim <- cbind(sim$X[, 1], sim$Y[, 1]) |> `colnames<-`(c("X", "Y"))
      if (M > 1) sim <- lapply(1:M, function(i) cbind(sim$X[, i], sim$Y[, i]) |> `colnames<-`(c("X", "Y")))
    }

    if (length(x0) == 3) {
      if (M == 1) sim <- cbind(sim$X[, 1], sim$Y[, 1], sim$Z[, 1]) |> `colnames<-`(c("X", "Y", "Z"))
      if (M > 1) sim <- lapply(1:M, function(i) cbind(sim$X[, i], sim$Y[, i], sim$Z[, i]) |> `colnames<-`(c("X", "Y", "Z")))
    }
  }

  return(sim)
}

#' Simulate multiple 1-3D Markovian Stochastic Differential Equations
#'
#' Simulate multiple Monte Carlo simulations of 1-3D Markovian Stochastic Differential Equations from a grid or random sample of initial values.
#' Parallel processing is supported. To register a parallel backend, use `future::plan()`. For example, `future::plan(future::multisession)`. For more information, see [future::plan()]. Functions imported from other programming languages, such as C++ or Python functions, may not work in parallel processing.
#' If you are uncertain whether there are unknown stable states of the system that are difficult to reach, it is recommended to start with running a large number (i.e., increasing `R`) of short simulations to see if the system reaches to the known stable states.
#'
#' @param sim_fun The simulation function to use. It should accept an argument `x0` for the initial values. Other arguments can be passed through `...`.
#' @param R The number of initial values to sample. If `sample_mode` is "grid", this will be the number of initial values in each dimension. If `sample_mode` is "random", this will be the total number of initial values.
#' @param range_x0 The range of initial values to sample in a vector of length 2 for each dimension (i.e., `c(<x0_minimum>, <x0_maximum>, <y0_minimum>, <y0_maximum>, <z0_minimum>, <z0_maximum>)`).
#' @param sample_mode The mode of sampling initial values. Either "grid" or "random". If "grid", the initial values will be sampled from a grid. If "random", the initial values will be sampled randomly.
#' @param ... Additional arguments passed to `sim_fun`.
#' @param .furrr_options A list of options to be passed to [furrr::future_pmap()].
#' @param return_object The type of object to return. Either "mcmc.list" or "raw". If "mcmc.list", a list of mcmc objects will be returned. If "raw", a tibble of initial values and raw simulation results will be returned.
#'
#' @return A list of mcmc objects or a tibble of initial values and raw simulation results, depending on the value of `return_object`.
#' @export
#'
#' @examples
#'
#' # Adapted from the example in the Sim.DiffProc package
#'
#' set.seed(1234, kind = "L'Ecuyer-CMRG")
#' mu <- 4
#' sigma <- 0.1
#' fx <- expression(y, (mu * (1 - x^2) * y - x))
#' gx <- expression(0, 2 * sigma)
#'
#' multiple_mod2d <- multi_init_simulation(sim_SDE, range_x0 = c(-3, 3, -10, 10),
#' R = 3, sample_mode = "grid", drift = fx, diffusion = gx,
#' N = 10000, Dt = 0.01, type = "str", method = "rk1",
#' keep_full = FALSE, M = 2)
#'
#' # The output is a mcmc.list object. You can use the functions
#' # in the coda package to modify it and perform convergence check,
#' # for example,
#'
#' library(coda)
#' plot(multiple_mod2d)
#' window(multiple_mod2d, start = 2000)
#' effectiveSize(multiple_mod2d)
#'
multi_init_simulation <- function(sim_fun, R = 10, range_x0, sample_mode = c("grid", "random"), ..., .furrr_options = list(.options = furrr::furrr_options(seed = TRUE)), return_object = c("mcmc.list", "raw")) {
  n_dim <- length(range_x0) / 2

  sample_mode <- match.arg(sample_mode)
  return_object <- match.arg(return_object)

  if (sample_mode == "grid") {
    # Create sequences for each dimension dynamically
    ranges <- lapply(seq_len(n_dim), function(i) {
      seq(range_x0[2 * i - 1], range_x0[2 * i], length.out = R)
    })

    # Generate a grid with dynamic variable names
    x0_all <- do.call(tidyr::expand_grid, stats::setNames(ranges, paste0("dim", seq_len(n_dim))))
  }

  if (sample_mode == "random") {
    # Generate random samples for each dimension dynamically
    ranges <- lapply(seq_len(n_dim), function(i) {
      stats::runif(R, min = range_x0[2 * i - 1], max = range_x0[2 * i])
    })

    # Create a tibble with dynamic variable names
    x0_all <- tibble::as_tibble(stats::setNames(ranges, paste0("dim", seq_len(n_dim))))
  }


  .sim_fun_options <- list(...)

  # Run the simulation with furrr::future_pmap. For each row of x0_all, the function sim_fun is called with a named argument x0, which is a vector of initial values, and the other arguments are passed through .sim_fun_options. The other arguments are passed to furrr::future_pmap() through .furrr_options. For those two options use do.call() to pass the list of options.

  sim_all <- do.call(
    furrr::future_pmap,
    c(
      list(
        x0_all,
        function(...) {
          do.call(
            sim_fun,
            c(
              list(x0 = c(...)),
              .sim_fun_options
            )
          )
        }
      ),
      .furrr_options
    )
  )

  if (return_object == "mcmc.list") {
    tryCatch({
    	return(as.mcmc.list.list(sim_all))},
    	error = function(e) {
    		warning("The output is not in a format that can be coerced to a mcmc.list object. Returning a raw output instead.")
    		return_object <- "raw"
    	})
    }

  if (return_object == "raw") {
    x0_all$output <- sim_all
    class(x0_all) <- c("multi_init_simulation", class(x0_all))
    return(x0_all)
  }
}




#' Convert a list of simulation output to a mcmc.list object
#'
#' This function can be used to convert a list of simulation output to a mcmc.list object. This may be useful when the output of the simulation is a list of matrices, and you want to perform convergence checks using the functions in the coda package. See [coda::mcmc.list()] for more information, and also see the examples in the documentation of [sim_SDE()].
#'
#' @param x A list of simulation output
#' @param ... Not used
#'
#' @return A mcmc.list object
#' @export
#'
#' @method as.mcmc.list list
#'
as.mcmc.list.list <- function(x, ...) {
	# Check if each element of sim_all is either a matrix, or can be coerced to a matrix, or is a list of matrices, or can be coerced to a list of matrices. If any of these conditions are met, first coerce everything to matrices, unnest nested lists (if any), make a list of matrices, convert each of them to mcmc objects and return a mcmc.list object out of those mcmc objects.
	sim_all <- x

	if (all(sapply(sim_all, function(x) is.list(x)))) {
		sim_all <- unlist(sim_all, recursive = FALSE)
	}
	if (all(sapply(sim_all, function(x) is.matrix(x) || coda::is.mcmc(x)))) {
		sim_all <- lapply(sim_all, function(x) {
			if (is.matrix(x)) {
				x <- coda::as.mcmc(x)
			}
			return(x)
		})
		sim_all <- coda::mcmc.list(sim_all)

		return(sim_all)
	} else {
		stop("The output is not in a format that can be coerced to a mcmc.list object. Returning a raw output instead.")
	}
}
