#' Create and modify variable sets for batch simulation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because we decided to shift to a more consistent
#' terminology. Previous `par` is renamed as `arg` (argument) and previous `var` is renamed
#' as `ele` (element). For creating an `arg_set` function, please use [new_arg_set()].
#'
#' A variable set contains the descriptions of the relevant variables in a
#' batch simulation. Use \code{new_var_set} to create a \code{var_set}
#' object, and use \code{add_var} to add descriptions of variables.
#'
#' @describeIn new_var_set Create a \code{var_set}.
#'
#' @param var_set A \code{var_set} object.
#' @param par_name,var_name The name of the parameter and variable
#' in the simulation function
#' @param start,end,by The data points where you want to test the variables.
#' Passed to \code{seq}.
#'
#' @return A \code{var_set} object.
#'
#' @examples
#' test <- new_var_set()
#' test <- test %>%
#'   add_var("par1", "var1", 1, 2, 0.1) %>%
#'   add_var("par2", "var2", 1, 2, 0.1)
#' @seealso \code{\link{make_var_grid}} for making grids from variable sets;
#' \code{\link{batch_simulation}} for running batch simulation and a
#' concrete example.
#'
#' @export
#' @export
new_var_set <- function() {
  lifecycle::deprecate_warn("0.2.0", "new_var_set()", "new_arg_set()")
  result <- list()
  class(result) <- c("var_set", "list")
  attr(result, "nvar") <- 0
  attr(result, "npar") <- 0
  return(result)
}

#' @describeIn new_var_set Add a variable to the `var_set`.
#' @export
add_var <- function(var_set, par_name, var_name, start, end, by) {
  lifecycle::deprecate_warn("0.2.0", "add_var()", "add_arg_ele()")
  var_set[[par_name]][[var_name]] <- tibble::lst(start, end, by) # <U+8FD9><U+91CC>var_name<U+5E94><U+8BE5><U+76F4><U+63A5><U+53D8><U+6210>name<U+5427><U+FF1F>
  attr(var_set, "nvar") <- attr(var_set, "nvar") + 1
  attr(var_set, "npar") <- length(var_set)
  return(var_set)
}

#' The number of variables in a `var_set`.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated. See [new_var_set()].
#'
#' @param var_set A `var_set` object.
#' @return An integer.
#' @export
nvar <- function(var_set) {
  lifecycle::deprecate_warn("0.2.0", "nvar()", "nele()")
  attr(var_set, "nvar")
}

#' The number of parameters in a `var_set`.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated. See [new_var_set()].
#'
#' @param var_set A `var_set` object.
#' @return An integer.
#' @export
npar <- function(var_set) {
  lifecycle::deprecate_warn("0.2.0", "npar()", "narg()")
  attr(var_set, "npar")
}

#' Print a `var_set` object.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated. See [new_var_set()].
#'
#' @param x The object.
#' @param detail Do you want to print the object details as a full list?
#' @param ... Not in use.
#' @return The printed result.
#' @method print var_set
#' @export
print.var_set <- function(x, detail = FALSE, ...) {
  lifecycle::deprecate_warn("0.2.0", "print.var_set()", "print.arg_set()")
  if (detail) {
    print.default(x)
  } else {
    cat(
      sprintf("A `var_set` with %d parameter(s) and %d variable(s)", npar(x), nvar(x))
    )
  }
}

#' Make variable grids for batch simulation
#'
#' This is the main function for making the variable grids.
#'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated. See [new_var_set()].
#'
#' @param var_set A `var_set` object. See [new_var_set()]
#' and [add_var()].
#'
#' @return A `var_grid` object.
#'
#' @seealso [batch_simulation()] for a concrete example.
#'
#' @export
make_var_grid <- function(var_set) {
  lifecycle::deprecate_warn("0.2.0", "make_var_grid()", "make_arg_grid()")
  var_set_seq <- list()
  var_set_par <- list()
  for (i in names(var_set)) {
    for (j in names(var_set[[i]])) {
      var_set_seq[[j]] <- seq(var_set[[i]][[j]]$start, var_set[[i]][[j]]$end, var_set[[i]][[j]]$by)
      var_set_par[[j]] <- names(var_set)[i]
    }
  }
  var_grid_num <- expand.grid(var_set_seq)

  var_grid_list <- data.frame(var_list = rep(NA, nrow(var_grid_num)))

  var_grid_list$var_list <- apply(var_grid_num, 1, fill_in_struct, var_set)

  var_grid <- cbind(var_grid_list, as.data.frame(var_grid_num))

  result <- var_grid
  class(result) <- c("var_grid", "data.frame")
  attr(result, "var_set_seq") <- var_set_seq
  attr(result, "var_set_par") <- var_set_par
  attr(result, "nvar") <- nvar(var_set)
  attr(result, "npar") <- npar(var_set)
  return(result)
}

#' Print a `var_grid` object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated. See [new_var_set()].
#'
#' @inheritParams print.var_set
#' @return The printed result.
#' @method print var_grid
#' @export
print.var_grid <- function(x, detail = FALSE, ...) {
  lifecycle::deprecate_warn("0.2.0", "print.var_grid()", "print.arg_grid()")
  if (detail) print.default(x)
  cat(
    sprintf(
      "A `var_grid` with %d parameter(s), %d variable(s), and %d condition(s)",
      npar(x), nvar(x), length(x$var_list)
    )
  )
}
