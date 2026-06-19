# Simulate multiple 1-3D Markovian Stochastic Differential Equations

Simulate multiple Monte Carlo simulations of 1-3D Markovian Stochastic
Differential Equations from a grid or random sample of initial values.
Parallel processing is supported. To register a parallel backend, use
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
For example, `future::plan(future::multisession)`. For more information,
see
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
Functions imported from other programming languages, such as C++ or
Python functions, may not work in parallel processing. If you are
uncertain whether there are unknown stable states of the system that are
difficult to reach, it is recommended to start with running a large
number (i.e., increasing `R`) of short simulations to see if the system
reaches to the known stable states.

## Usage

``` r
multi_init_simulation(
  sim_fun,
  R = 10,
  range_x0,
  sample_mode = c("grid", "random"),
  ...,
  .furrr_options = list(.options = furrr::furrr_options(seed = TRUE)),
  return_object = c("mcmc.list", "raw")
)
```

## Arguments

- sim_fun:

  The simulation function to use. It should accept an argument `x0` for
  the initial values. Other arguments can be passed through `...`.

- R:

  The number of initial values to sample. If `sample_mode` is "grid",
  this will be the number of initial values in each dimension. If
  `sample_mode` is "random", this will be the total number of initial
  values.

- range_x0:

  The range of initial values to sample in a vector of length 2 for each
  dimension (i.e.,
  `c(<x0_minimum>, <x0_maximum>, <y0_minimum>, <y0_maximum>, <z0_minimum>, <z0_maximum>)`).

- sample_mode:

  The mode of sampling initial values. Either "grid" or "random". If
  "grid", the initial values will be sampled from a grid. If "random",
  the initial values will be sampled randomly.

- ...:

  Additional arguments passed to `sim_fun`.

- .furrr_options:

  A list of options to be passed to
  [`furrr::future_pmap()`](https://furrr.futureverse.org/reference/future_map2.html).

- return_object:

  The type of object to return. Either "mcmc.list" or "raw". If
  "mcmc.list", a list of mcmc objects will be returned. If "raw", a
  tibble of initial values and raw simulation results will be returned.

## Value

A list of mcmc objects or a tibble of initial values and raw simulation
results, depending on the value of `return_object`.

## Examples

``` r

# Adapted from the example in the Sim.DiffProc package

set.seed(1234, kind = "L'Ecuyer-CMRG")
mu <- 4
sigma <- 0.1
fx <- expression(y, (mu * (1 - x^2) * y - x))
gx <- expression(0, 2 * sigma)

multiple_mod2d <- multi_init_simulation(sim_SDE, range_x0 = c(-3, 3, -10, 10),
R = 3, sample_mode = "grid", drift = fx, diffusion = gx,
N = 1000, Dt = 0.01, type = "str", method = "rk1",
keep_full = FALSE, M = 2)
#> Error in eval(drifty): object 'mu' not found

# The output is a mcmc.list object. You can use the functions
# in the coda package to modify it and perform convergence check,
# for example,

library(coda)
plot(multiple_mod2d)
#> Error: object 'multiple_mod2d' not found
window(multiple_mod2d, start = 500)
#> Error: object 'multiple_mod2d' not found
effectiveSize(multiple_mod2d)
#> Error: object 'multiple_mod2d' not found
```
