# Make a grid for calculating barriers for 3d landscapes

Make a grid for calculating barriers for 3d landscapes

## Usage

``` r
make_barrier_grid_3d(
  ag,
  start_location_value,
  start_r,
  end_location_value,
  end_r,
  df = NULL,
  print_template = FALSE
)
```

## Arguments

- ag:

  An `arg_grid` object.

- start_location_value, start_r, end_location_value, end_r:

  Default values for finding local minimum. See
  [`calculate_barrier()`](https://sciurus365.github.io/simlandr/reference/calculate_barrier.md).

- df:

  A data frame for the variables. Use `print_template = TRUE` to get a
  template.

- print_template:

  Print a template for `df`.

## Value

A `barrier_grid_3d` object that specifies the condition for each barrier
calculation.
