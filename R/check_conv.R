add_stage_tag <- function(output, range, var, stage) {
  if (missing(range)) range <- 1:nrow(output)
  output <- cbind(output[range, ] %>% as.data.frame() %>% dplyr::select(var), stage)
  colnames(output)[ncol(output)] <- "stage"
  return(output)
}

#' Graphical diagnoses to check if the simulation converges
#'
#' Compare the distribution of different stages of simulation (for `plot_type == "bin"` or `plot_type = "density"`), or show how the percentails of the distribution evolve over time (for `plot_type == cumuplot`, see [coda::cumuplot()] for details). More convergence checking methods for MCMC data are available at the `coda` package. Be cautious: each convergence checking method has its shortcomings, so do not blindly use any results as the definitive conclusion that a simulation converges or not.
#'
#'
#'
#' @param output A matrix of simulation output.
#' @param vars The names of variables to check.
#' @param sample_perc The percentage of data sample for the initial, middle, and final stage of the simulation. Not required if `plot_type == "cumuplot"`.
#' @param plot_type Which type of plots should be generated? ("bin", "density", or "cumuplot" which uses [coda::cumuplot()])
#' @param ... Other parameters to be passed to [coda::cumuplot()].
#'
#' @return A `check_conv` object that contains the convergence checking result(for `plot_type == "bin"` or `plot_type = "density"`), or draw the cumuplot without a return value (for `plot_type == cumuplot`).
#'
#' @export
check_conv <- function(output, vars, sample_perc = 0.2, plot_type = "bin") {
  if (plot_type == "cumuplot") {
    rlang::check_installed("coda", reason = "to use `plot_type = `cumuplot`")
    return(coda::cumuplot(coda::mcmc(output[, vars])))
  }

  # check convergence of i in vars; init, mid, final, normalized dist, ...
  if (sample_perc > 1 | sample_perc < 0) stop("`sample_perc should be between 0 and 1.")

  result_list <- list()
  simulation_length <- nrow(output)

  for (i in vars) {
    stage_list <- vector("list", 3)
    stage_list[[1]] <- output %>% add_stage_tag(1:(sample_perc * simulation_length), i, "initial")
    stage_list[[2]] <- output %>% add_stage_tag(((0.5 - 0.5 * sample_perc) * simulation_length):((0.5 + 0.5 * sample_perc) * simulation_length), i, "middle")
    stage_list[[3]] <- output %>% add_stage_tag(((1 - sample_perc) * simulation_length):simulation_length, i, stage = "final")
    # stage_list[[4]] <- output %>% add_stage_tag(1:simulation_length, i, stage = "all")

    data_all <- do.call(rbind, stage_list) %>% dplyr::mutate(stage = forcats::fct_relevel(stage, "initial", "middle", "final"))

    if (plot_type == "bin") {
      p <- ggplot2::ggplot(data_all, mapping = ggplot2::aes(x = !!rlang::sym(i), fill = stage)) +
        ggplot2::stat_bin(position = "dodge") +
        ggplot2::labs(x = i) +
        ggplot2::theme_bw()
    } else if (plot_type == "density") {
      p <- ggplot2::ggplot(data_all, mapping = ggplot2::aes(x = !!rlang::sym(i), color = stage)) +
        ggplot2::geom_density() +
        ggplot2::labs(x = i) +
        ggplot2::theme_bw()
    } else {
      stop("'plot_type` should be either 'bin' or 'density'.")
    }

    result_list[[i]] <- p
  }
  class(result_list) <- c("check_conv", "list")
  return(result_list)
}

#' @describeIn check_conv Print a `check_conv` object.
#' @param x The object.
#' @param ask Ask to press enter to see the next plot?
#' @param ... Not in use.
#' @method print check_conv
#' @export
print.check_conv <- function(x, ask = TRUE, ...) {
  if (ask) {
    for (i in x) {
      print(i)
      readline(prompt = "Press <Enter> to see the next plot...")
    }
  } else {
    print.default(x)
  }
}
