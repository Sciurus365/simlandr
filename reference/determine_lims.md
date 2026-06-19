# Determine plot limits from simulated output

Internal helper used by landscape constructors and downstream packages.

## Usage

``` r
determine_lims(output, var_names, lims)
```

## Arguments

- output:

  A matrix of simulation output, or a list containing one.

- var_names:

  The target variable names.

- lims:

  User-supplied limits, if any.

## Value

A numeric vector of axis limits.
