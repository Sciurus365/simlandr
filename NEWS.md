# Development version

-		Further improved documentations, function and class names, and cleaned unnecessary exported functions; added `print()` and `plot()` methods for several classes.
-		Renamed `hash_big.matrix` class to `hash_big_matrix` for consistent use of dots; renamed related functions accordingly.

# simlandr 0.2.0

-   Renamed `var` and `par` in batch simulation functions to `arg` (argument) and `ele` (element).
-   Adjusted the algorithm of calculating minimal energy paths (previously "minimal elevation paths").
-   Added an nongradient example function `sim_fun_nongrad()`; renamed `sim_fun_test2()` to `sim_fun_grad()`.
-   Added title for the color bar in `plot_ly` based plots; used `theme_bw()` throughout all `ggplot2` based plots.
-   Improved some documentations; included a new vignette.

# simlandr 0.1.2

-   Improved the manual.
-   Added `sim_fun_test2` function for testing.
-   Added `...` for `batch_simulation()`.
-   Added border for white dots in the geom of `calculate_barrier_2d()` and `calculate_barrier_2d_batch()`.

# simlandr 0.1.1

-   Cleaned the package to make it CRAN-compatible.

# simlandr 0.1.0

-   Initial release.
