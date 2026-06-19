# Graphical diagnoses to check if the simulation converges

Compare the distribution of different stages of simulation (for
`plot_type == "bin"` or `plot_type = "density"`), or show how the
percentiles of the distribution evolve over time (for
`plot_type == cumuplot`, see
[`coda::cumuplot()`](https://rdrr.io/pkg/coda/man/cumuplot.html) for
details). More convergence checking methods for MCMC data are available
at the `coda` package. Be cautious: each convergence checking method has
its shortcomings, so do not blindly use any results as the definitive
conclusion that a simulation converges or not.

## Usage

``` r
check_conv(output, vars, sample_perc = 0.2, plot_type = "bin")

# S3 method for class 'check_conv'
print(x, ask = TRUE, ...)
```

## Arguments

- output:

  A matrix of simulation output, or a `multi_init_simulation` object
  generated from
  [`multi_init_simulation()`](https://sciurus365.github.io/simlandr/reference/multi_init_simulation.md).

- vars:

  The names of variables to check.

- sample_perc:

  The percentage of data sample for the initial, middle, and final stage
  of the simulation. Not required if `plot_type == "cumuplot"`.

- plot_type:

  Which type of plots should be generated? ("bin", "density", or
  "cumuplot" which uses
  [`coda::cumuplot()`](https://rdrr.io/pkg/coda/man/cumuplot.html))

- x:

  The object.

- ask:

  Ask to press enter to see the next plot?

- ...:

  Not in use.

## Value

A `check_conv` object that contains the convergence checking result(for
`plot_type == "bin"` or `plot_type = "density"`), or draw the cumuplot
without a return value (for `plot_type == cumuplot`).

## Methods (by generic)

- `print(check_conv)`: Print a `check_conv` object.
