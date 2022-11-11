## Changes of simlandr 0.3.0

- Further improved documentations, function and class names, and cleaned unnecessary exported functions. Removed some deprecated functions.
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

## Test environments

- Github R-CMD-check MacOS R 4.2.2
- Github R-CMD-check Windows R 4.2.2
- Github R-CMD-check Ubuntu R-devel
- Github R-CMD-check Ubuntu R 4.2.2
- Github R-CMD-check Ubuntu R 4.1.3

## R CMD check results

0 errors | 0 warnings | 0 note
