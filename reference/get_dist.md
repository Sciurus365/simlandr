# Get the probability distribution from a landscape object

Get the probability distribution from a landscape object

## Usage

``` r
get_dist(l, index = 1)
```

## Arguments

- l:

  A `landscape` project.

- index:

  1 to get the distribution in tidy format; 2 or "raw" to get the raw
  simulation result (`batch_simulation`).

## Value

A `data.frame` that contains the distribution in the tidy format or the
raw simulation result.
