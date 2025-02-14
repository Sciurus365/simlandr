# simlandr 0.4.0

- Added simulation helper functions `sim_SDE()` and `multi_init_simulation()` for simulating stochastic differential equations (SDEs) and multiple initial conditions, respectively.
- Added `as.mcmc.list()` method for lists of simulation outputs. The added simulation functions also have the option to return the output as an `mcmc` or `mcmc.list` object, so that they can be used with `coda` functions.
- The landscape estimation functions now get a new parameter `weight_var` to specify the variable that contains the weight of each sample point. This may be useful if user applies importance sampling.
- Added vignettes and more examples.

# simlandr 0.3.1

- Removed unused parameter `Umax` from the documentation of `make_kernel_dist()` (an internal function).

# simlandr 0.3.0

- Further improved documentation, function, and class names, and cleaned unnecessary exported functions. Removed some deprecated functions.
- Made the parameter names of the landscape functions more consistent; used better default values for landscape functions.
- For single simulation landscape functions, added `make_*d_single()` alias.
- Removed some default values for barrier calculation functions (they are often not suitable and can be misleading).
- Added `print()`, `summary()` and `plot()` methods for several classes.
- Replaced `get_geom()` with a method of the `autolayer()` generic function from `ggplot2`. `get_geom()` is now deprecated.
- Replaced `get_barrier_height()` with a method of the `summary()` generic function. `get_barrier_height()` is now deprecated.
- Renamed `hash_big.matrix` class to `hash_big_matrix` for consistent use of dots; renamed related functions accordingly.
- Added `coda::cumuplot()` as a possible option for `check_conv()`.
- For `barrier_batch` objects, renamed the column `b` to `barrier` to avoid possible conflicts.
- Removed some unnecessary messages.
- Added some examples to `README`.
- Updated the vignette: updated some function, object, and parameter names; removed some references that we are now questioning.
- Bug fix: Parameter `vg` in `make_barrier_grid_2d()` and `make_barrier_grid_3d()` was changed to `ag` and the class of this parameter was changed from `var_grid` (deprecated) to `arg_grid`. 

# simlandr 0.2.1

- Updated roxygen2 version to ensure html5 compatibility (>= R 4.2.0).

# simlandr 0.2.0

- Renamed `var` and `par` in batch simulation functions to `arg` (argument) and `ele` (element).
- Adjusted the algorithm of calculating minimal energy paths (previously "minimal elevation paths").
- Added an non-gradient example function `sim_fun_nongrad()`; renamed `sim_fun_test2()` to `sim_fun_grad()`.
- Added title for the color bar in `plot_ly` based plots; used `theme_bw()` throughout all `ggplot2` based plots.
- Improved some documentation; included a new vignette.

# simlandr 0.1.2

- Improved the manual.
- Added `sim_fun_test2` function for testing.
- Added `...` for `batch_simulation()`.
- Added border for white dots in the geom of `calculate_barrier_2d()` and `calculate_barrier_2d_batch()`.

# simlandr 0.1.1

- Cleaned the package to make it CRAN-compatible.

# simlandr 0.1.0

- Initial release.
