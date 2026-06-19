# Shared 3D barrier core

Internal helper used by `simlandr` and downstream packages to calculate
barrier heights.

## Usage

``` r
calculate_barrier_3d_core(
  d,
  x_label,
  y_label,
  start_location_value,
  start_r,
  end_location_value,
  end_r,
  Umax,
  expand = TRUE,
  omit_unstable = FALSE,
  base = exp(1)
)
```

## Arguments

- d:

  A distribution object with components `x`, `y`, and matrix `d`.

- x_label, y_label:

  Axis labels for plotting.

- start_location_value, end_location_value:

  The initial position (in value) for searching the start/end point.

- start_r, end_r:

  The search radius (in L1 distance) for the start/end point.

- Umax:

  The highest possible value of the potential function.

- expand:

  If the values in the range all equal to `Umax`, expand the range or
  not?

- omit_unstable:

  If a state is not stable (the "local minimum" overlaps with the saddle
  point), omit that state or not?

- base:

  The base of the log function.

## Value

A `3d_barrier` object.
