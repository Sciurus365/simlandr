# Functions for calculating energy barrier from landscapes

Functions for calculating energy barrier from landscapes

## Usage

``` r
calculate_barrier(l, ...)

# S3 method for class '`2d_landscape`'
calculate_barrier(
  l,
  start_location_value,
  start_r,
  end_location_value,
  end_r,
  base = exp(1),
  ...
)

# S3 method for class '`3d_landscape`'
calculate_barrier(
  l,
  start_location_value,
  start_r,
  end_location_value,
  end_r,
  Umax,
  expand = TRUE,
  omit_unstable = FALSE,
  base = exp(1),
  ...
)

# S3 method for class '`2d_landscape_batch`'
calculate_barrier(
  l,
  bg = NULL,
  start_location_value,
  start_r,
  end_location_value,
  end_r,
  base = exp(1),
  ...
)

# S3 method for class '`3d_landscape_batch`'
calculate_barrier(
  l,
  bg = NULL,
  start_location_value,
  start_r,
  end_location_value,
  end_r,
  Umax,
  expand = TRUE,
  omit_unstable = FALSE,
  base = exp(1),
  ...
)
```

## Arguments

- l:

  A `landscape` object.

- ...:

  Not in use.

- start_location_value, end_location_value:

  The initial position (in value) for searching the start/end point.

- start_r, end_r:

  The search radius (in L1 distance) for the start/end point.

- base:

  The base of the log function.

- Umax:

  The highest possible value of the potential function.

- expand:

  If the values in the range all equal to `Umax`, expand the range or
  not?

- omit_unstable:

  If a state is not stable (the "local minimum" overlaps with the saddle
  point), omit that state or not?

- bg:

  A `2d_barrier_grid` or `3d_barrier_grid` object if you want to use
  different parameters for each condition. Otherwise `NULL` as default.

## Value

A `barrier` object that contains the (batch) barrier calculation
result(s).
