#' Calculate 1D, 2D, or 3D kernel smooth distribution
#'
#' @param output A matrix of simulation output.
#' @param var_names The names of the target variables.
#' @param lims The limits of the range for the density estimator as `c(xl, xu)` for 2D landscapes, `c(xl, xu, yl, yu)` for 3D landscapes, `c(xl, xu, yl, yu, zl, zu)` for 4D landscapes. If missing, the range of the data extended by 10% for both sides will be used. For landscapes based on multiple simulations, the largest range of all simulations (which means the lowest lower limit and the highest upper limit) will be used by default.
#' @param kde_fun Which kernel estimator to use? Choices: "ks" [ks::kde()] (default; faster and using less memory); "base" `base::density()` (only for 2D landscapes); "MASS" [MASS::kde2d()] (only for 3D landscapes).
#' @param n The number of equally spaced points in each axis, at which the density is to be estimated.
#' @param h A number, or possibly a vector for 3D and 4D landscapes, specifying the smoothing bandwidth to be used. If missing, the default value of the kernel estimator will be used (but `bw = "SJ"` for `base::density()`). Note that the definition of bandwidth might be different for different kernel estimators. For landscapes based on multiple simulations, the largest `h` of all simulations will be used by default.
#' @param adjust The multiplier to the bandwidth. The bandwidth used is actually `adjust * h`. This makes it easy to specify values like "half the default" bandwidth.
#' @param Umax The maximum displayed value of potential.
#' @return A list of the smooth distribution.
#' @keywords internal
make_kernel_dist <- function(output, var_names, lims, kde_fun, n, h, adjust) {
  if (is.list(output)) output <- output[[1]]
  if (any(!is.finite(output[, var_names]))) {
    return(NULL)
  }

  if (kde_fun == "ks") {
    # prepare the parameters for ks::kde
    output_x <- output[, var_names]
    gridsize <- rep(n, length(var_names))
    xmin <- lims[seq(from = 1, by = 2, length = length(var_names))]
    xmax <- lims[seq(from = 2, by = 2, length = length(var_names))]
    # calculate the result using ks::kde
    if (length(var_names) == 1) {
      result_raw <- ks::kde(output_x, h = h, gridsize = gridsize, xmin = xmin, xmax = xmax, compute.cont = FALSE, approx.cont = FALSE)
    } else {
      result_raw <- ks::kde(output_x, H = h, gridsize = gridsize, xmin = xmin, xmax = xmax, compute.cont = FALSE, approx.cont = FALSE)
    }
    # reformat the result

    if (length(var_names) == 1) {
      result <- list(x = result_raw$eval.points, d = pmax(result_raw$estimate, 0))
    } else if (length(var_names) == 2) {
      result <- list(x = result_raw$eval.points[[1]], y = result_raw$eval.points[[2]], d = pmax(result_raw$estimate, 0))
    } else if (length(var_names) == 3) result <- list(x = result_raw$eval.points[[1]], y = result_raw$eval.points[[2]], z = result_raw$eval.points[[3]], d = pmax(result_raw$estimate, 0))
  } else if (kde_fun == "base") {
    if (length(var_names) != 1) stop('kde_fun = "MASS" can only be used for 2D landscapes.')
    x <- var_names
    result <- stats::density(output[, x], n = n, bw = h, from = lims[1], to = lims[2])
    result <- list(x = result$x, d = result$y)
  } else if (kde_fun == "MASS") {
    if (length(var_names) != 2) stop('kde_fun = "MASS" can only be used for 3D landscapes.')
    x <- var_names[1]
    y <- var_names[2]
    result <- MASS::kde2d(x = output[, x], y = output[, y], n = n, lims = lims, h = h)
    result <- list(x = result$x, y = result$y, d = result$z)
  } else {
    stop('Wrong input for `kde_fun`. Please choose from "ks" and "MASS".')
  }

  return(result)
}


determine_h <- function(output, var_names, kde_fun, h, adjust) {
  if (is.list(output)) output <- output[[1]]
  if (kde_fun == "ks") {
    output_x <- output[, var_names]
    if (length(var_names) == 1) {
      h <- ifelse(rlang::is_missing(h), ks::hpi(output_x) * adjust, h * adjust)
    } else if (rlang::is_missing(h)) {
      h <- ks::Hpi(output_x) * adjust
    } else if (is.matrix(h)) {
      h <- h * adjust
    } else {
      h <- diag(h, length(var_names), length(var_names)) * adjust
    }
  } else if (kde_fun == "MASS") {
    if (length(var_names) != 2) stop('kde_fun = "MASS" can only be used for 3D landscapes.')
    x <- var_names[1]
    y <- var_names[2]
    if (rlang::is_missing(h)) {
      h <- c(MASS::bandwidth.nrd(output[, x]), MASS::bandwidth.nrd(output[, y])) * adjust
    } else {
      h <- h * adjust
    }
  } else if (kde_fun == "base") {
    if (length(var_names) != 1) stop('kde_fun = "MASS" can only be used for 2D landscapes.')
    x <- var_names
    h <- ifelse(rlang::is_missing(h), stats::bw.SJ(output[, x]), h) * adjust
  }

  return(h)
}

determine_lims <- function(output, var_names, lims) {
  if (!rlang::is_missing(lims)) {
    return(lims)
  }
  if (is.list(output)) output <- output[[1]]
  if (rlang::is_missing(lims)) {
    return(c(sapply(var_names, function(v) grDevices::extendrange(output[, v], f = 0.1))))
  }
  if (any(is.infinite(lims))) stop("Non-infinite values found in `lims`.")
}


determine_h_batch <- function(bs, var_names, kde_fun, h, adjust) {
  h_batch <- lapply(bs$output, determine_h, var_names, kde_fun, h, adjust)
  h <- do.call(pmax, h_batch)
  return(h)
}

determine_lims_batch <- function(bs, var_names, lims) {
  if (!rlang::is_missing(lims)) {
    return(lims)
  }
  lims_batch <- lapply(bs$output, determine_lims, var_names, lims) %>%
    unlist() %>%
    matrix(byrow = TRUE, nrow = nrow(bs))
  lims <- vector("numeric", ncol(lims_batch))
  for (i in 1:length(lims)) {
    if (i %% 2 == 1) {
      lims[i] <- min(lims_batch[, i])
    } else {
      lims[i] <- max(lims_batch[, i])
    }
  }
  return(lims)
}

#' Make a tidy `data.frame` from smooth 2d distribution matrix
#'
#' @param dist_2d `kde2d` distribution.
#' @param value The value of the variable of interest.
#' @param var_name The name of the variable.
#'
#' @return A tidy `data.frame`.
#' @export
#' @keywords internal
make_2d_tidy_dist <- function(dist_2d, value = NULL, var_name = NULL) {
  df <- cbind(
    expand.grid(x = dist_2d$x, y = dist_2d$y),
    expand.grid(
      x_index = 1:length(dist_2d$x),
      y_index = 1:length(dist_2d$y)
    )
  )
  d_mat <- dist_2d$d
  df <- df %>%
    dplyr::mutate(d = purrr::map2_dbl(x_index, y_index, function(x, y, dm) {
      dm[x, y]
    }, dm = d_mat))

  if (!is.null(value) & !is.null(var_name)) df[, var_name] <- value
  return(df)
}

#' Make a tidy `data.frame` from smooth 3d distribution matrix
#'
#' @param dist_3d `kde2d`-type distribution.
#' @param value The value of the variable of interest.
#' @param var_name The name of the variable.
#'
#' @return A tidy `data.frame`.
#' @noRd
make_3d_tidy_dist <- function(dist_3d, value = NULL, var_name = NULL) {
  df <- cbind(
    expand.grid(x = dist_3d$x, y = dist_3d$y, z = dist_3d$z),
    expand.grid(
      x_index = 1:length(dist_3d$x),
      y_index = 1:length(dist_3d$y),
      z_index = 1:length(dist_3d$z)
    )
  )
  d_mat <- dist_3d$d
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(d = d_mat[x_index, y_index, z_index]) %>%
    dplyr::ungroup()

  if (!is.null(value) & !is.null(var_name)) df[, var_name] <- value
  return(df)
}
