add_stage_tag <- function(output, range, var, stage){
	if(missing(range)) range <- 1:nrow(output)
	output <- cbind(output[range, ] %>% as.data.frame %>% dplyr::select(var), stage)
	colnames(output)[ncol(output)] <- "stage"
	return(output)
}

#' Check density convergence of simulation
#'
#' @param output A matrix of simulation output.
#' @param vars The names of variables to check.
#' @param sample_perc The percentage of data sample for the initial, middle, and final stage of the simulation.
#'
#' @export
check_conv <- function(output, vars, sample_perc = 0.2){
	# check convergence of i in vars; init, mid, final, normalized dist, ...
	if(sample_perc > 1 | sample_perc < 0) stop("`sample_perc should be between 0 and 1.")

	result_list <- list()
	simulation_length <- nrow(output)

	for(i in vars){
		stage_list <- vector("list", 3)
		stage_list[[1]] <- output %>% add_stage_tag(1:(sample_perc*simulation_length), i, "initial")
		stage_list[[2]] <- output %>% add_stage_tag(((0.5-0.5*sample_perc)*simulation_length):((0.5+0.5*sample_perc)*simulation_length), i, "middle")
		stage_list[[3]] <- output %>% add_stage_tag(((1-sample_perc)*simulation_length):simulation_length, i, stage = "final")
		# stage_list[[4]] <- output %>% add_stage_tag(1:simulation_length, i, stage = "all")

		data_all <- do.call(rbind, stage_list) %>% dplyr::mutate(stage = forcats::fct_relevel(stage, "initial", "middle", "final"))

		p <- ggplot2::ggplot(data_all, mapping = ggplot2::aes(x = !!rlang::sym(i), fill = stage)) +
			ggplot2::stat_bin(position = "dodge") +
			ggplot2::labs(x = i)
		result_list[[i]] <- p
	}
	class(result_list) <- c("check_conv","list")
	return(result_list)
}

#' Print a `check_conv`
#' @param x The object.
#' @param ask Ask to press enter to see the next plot?
#' @param ... Not in use.
#' @method print check_conv
#' @export
print.check_conv <- function(x, ask = TRUE, ...){
	if(ask){
		for(i in x){
			print(i)
			readline(prompt = "Press <Enter> to see the next plot...")
		}
	}else{
		print.default(x)
	}
}
