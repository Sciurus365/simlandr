# Perform a batch simulation.

Perform a batch simulation.

## Usage

``` r
batch_simulation(
  arg_grid,
  sim_fun,
  default_list = list(),
  bigmemory = TRUE,
  ...
)

# S3 method for class 'batch_simulation'
print(x, detail = FALSE, ...)
```

## Arguments

- arg_grid:

  An `arg_grid` object. See
  [`make_arg_grid()`](https://sciurus365.github.io/simlandr/reference/arg_set-class.md).

- sim_fun:

  The simulation function. See
  [`sim_fun_test()`](https://sciurus365.github.io/simlandr/reference/sim_fun_test.md)
  for an example.

- default_list:

  A list of default values for `sim_fun`.

- bigmemory:

  Use
  [`hash_big_matrix-class()`](https://sciurus365.github.io/simlandr/reference/hash_big_matrix-class.md)
  to store large matrices?

- ...:

  Other parameters passed to `sim_fun`

- x:

  An `arg_set` object

- detail:

  Do you want to print the object details as a full list?

## Value

A `batch_simulation` object, also a data frame. The first column, `var`,
is a list of `ele_list` that contains all the variables; the second to
the second last columns are the values of the variables; the last column
is the output of the simulation function.

## Functions

- `batch_simulation()`: Perform a batch simulation.

## Examples

``` r
batch_arg_set_grad <- new_arg_set()
batch_arg_set_grad <- batch_arg_set_grad %>%
  add_arg_ele(
    arg_name = "parameter", ele_name = "a",
    start = -6, end = -1, by = 1
  )
batch_grid_grad <- make_arg_grid(batch_arg_set_grad)
batch_output_grad <- batch_simulation(batch_grid_grad, sim_fun_grad,
  default_list = list(
    initial = list(x = 0, y = 0),
    parameter = list(a = -4, b = 0, c = 0, sigmasq = 1)
  ),
  length = 1e2,
  seed = 1614,
  bigmemory = FALSE
)
print(batch_output_grad)
#> Output(s) from 6 simulations.
```
