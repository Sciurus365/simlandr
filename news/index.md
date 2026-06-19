# Changelog

## simlandr 0.4.1

- Standardized landscape plotting: use
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  for ggplot output and
  [`plotly_ld()`](https://sciurus365.github.io/simlandr/reference/plotly_ld.md)
  for interactive plotly output. The previous
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
  remains available with a soft-deprecation warning.
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  is re-exported, so attaching `ggplot2` separately is not required.
- Modernized user-facing messages, warnings, and errors using `cli`.
- Used Rcpp for the Dijkstra algorithm to improve performance.
- Improved downstream portability by exporting several internal
  functions.
- Updated citation information.

## simlandr 0.4.0

CRAN release: 2025-02-12

- Added simulation helper functions
  [`sim_SDE()`](https://sciurus365.github.io/simlandr/reference/sim_SDE.md)
  and
  [`multi_init_simulation()`](https://sciurus365.github.io/simlandr/reference/multi_init_simulation.md)
  for simulating stochastic differential equations (SDEs) and multiple
  initial conditions, respectively.
- Added
  [`as.mcmc.list()`](https://sciurus365.github.io/simlandr/reference/as.mcmc.list.md)
  method for lists of simulation outputs. The added simulation functions
  also have the option to return the output as an `mcmc` or `mcmc.list`
  object, so that they can be used with `coda` functions.
- The landscape estimation functions now get a new parameter
  `weight_var` to specify the variable that contains the weight of each
  sample point. This may be useful if user applies importance sampling.
- Added vignettes and more examples.

## simlandr 0.3.1

CRAN release: 2024-01-23

- Removed unused parameter `Umax` from the documentation of
  [`make_kernel_dist()`](https://sciurus365.github.io/simlandr/reference/make_kernel_dist.md)
  (an internal function).

## simlandr 0.3.0

CRAN release: 2022-11-15

- Further improved documentation, function, and class names, and cleaned
  unnecessary exported functions. Removed some deprecated functions.
- Made the parameter names of the landscape functions more consistent;
  used better default values for landscape functions.
- For single simulation landscape functions, added `make_*d_single()`
  alias.
- Removed some default values for barrier calculation functions (they
  are often not suitable and can be misleading).
- Added [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for
  several classes.
- Replaced
  [`get_geom()`](https://sciurus365.github.io/simlandr/reference/get_geom.md)
  with a method of the
  [`autolayer()`](https://sciurus365.github.io/simlandr/reference/autolayer.md)
  generic function from `ggplot2`.
  [`get_geom()`](https://sciurus365.github.io/simlandr/reference/get_geom.md)
  is now deprecated.
- Replaced
  [`get_barrier_height()`](https://sciurus365.github.io/simlandr/reference/get_barrier_height.md)
  with a method of the
  [`summary()`](https://rdrr.io/r/base/summary.html) generic function.
  [`get_barrier_height()`](https://sciurus365.github.io/simlandr/reference/get_barrier_height.md)
  is now deprecated.
- Renamed `hash_big.matrix` class to `hash_big_matrix` for consistent
  use of dots; renamed related functions accordingly.
- Added [`coda::cumuplot()`](https://rdrr.io/pkg/coda/man/cumuplot.html)
  as a possible option for
  [`check_conv()`](https://sciurus365.github.io/simlandr/reference/check_conv.md).
- For `barrier_batch` objects, renamed the column `b` to `barrier` to
  avoid possible conflicts.
- Removed some unnecessary messages.
- Added some examples to `README`.
- Updated the vignette: updated some function, object, and parameter
  names; removed some references that we are now questioning.
- Bug fix: Parameter `vg` in
  [`make_barrier_grid_2d()`](https://sciurus365.github.io/simlandr/reference/make_barrier_grid_2d.md)
  and
  [`make_barrier_grid_3d()`](https://sciurus365.github.io/simlandr/reference/make_barrier_grid_3d.md)
  was changed to `ag` and the class of this parameter was changed from
  `var_grid` (deprecated) to `arg_grid`.

## simlandr 0.2.1

CRAN release: 2022-08-24

- Updated roxygen2 version to ensure html5 compatibility (\>= R 4.2.0).

## simlandr 0.2.0

CRAN release: 2022-03-16

- Renamed `var` and `par` in batch simulation functions to `arg`
  (argument) and `ele` (element).
- Adjusted the algorithm of calculating minimal energy paths (previously
  “minimal elevation paths”).
- Added an non-gradient example function
  [`sim_fun_nongrad()`](https://sciurus365.github.io/simlandr/reference/sim_fun_nongrad.md);
  renamed
  [`sim_fun_test2()`](https://sciurus365.github.io/simlandr/reference/sim_fun_test2.md)
  to
  [`sim_fun_grad()`](https://sciurus365.github.io/simlandr/reference/sim_fun_grad.md).
- Added title for the color bar in `plot_ly` based plots; used
  `theme_bw()` throughout all `ggplot2` based plots.
- Improved some documentation; included a new vignette.

## simlandr 0.1.2

CRAN release: 2021-11-02

- Improved the manual.
- Added `sim_fun_test2` function for testing.
- Added `...` for
  [`batch_simulation()`](https://sciurus365.github.io/simlandr/reference/batch_simulation.md).
- Added border for white dots in the geom of `calculate_barrier_2d()`
  and `calculate_barrier_2d_batch()`.

## simlandr 0.1.1

CRAN release: 2021-08-16

- Cleaned the package to make it CRAN-compatible.

## simlandr 0.1.0

- Initial release.
