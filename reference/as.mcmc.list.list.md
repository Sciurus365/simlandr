# Convert a list of simulation output to a mcmc.list object

This function can be used to convert a list of simulation output to a
mcmc.list object. This may be useful when the output of the simulation
is a list of matrices, and you want to perform convergence checks using
the functions in the coda package. See
[`coda::mcmc.list()`](https://rdrr.io/pkg/coda/man/mcmc.list.html) for
more information, and also see the examples in the documentation of
[`sim_SDE()`](https://sciurus365.github.io/simlandr/reference/sim_SDE.md).

## Usage

``` r
# S3 method for class 'list'
as.mcmc.list(x, ...)
```

## Arguments

- x:

  A list of simulation output

- ...:

  Not used

## Value

A mcmc.list object
