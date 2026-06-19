# Attach all matrices in a batch simulation

Attach all matrices in a batch simulation

## Usage

``` r
attach_all_matrices(bs, backingpath = "bp")
```

## Arguments

- bs:

  A `batch_simulation` object.

- backingpath:

  Passed to
  [`bigmemory::as.big.matrix()`](https://rdrr.io/pkg/bigmemory/man/big.matrix.html).

## Value

A `batch_simulation` object with all `hash_big_matrix`es attached.
