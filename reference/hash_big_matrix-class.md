# Class "hash_big_matrix": big matrix with a md5 hash reference

`hash_big_matrix` class is a modified class from
[`bigmemory::big.matrix-class()`](https://rdrr.io/pkg/bigmemory/man/big.matrix-class.html).
Its purpose is to help users operate big matrices within hard disk in a
reusable way, so that the large matrices do not consume too much memory,
and the matrices can be reused for the next time. Comparing with
[`bigmemory::big.matrix-class()`](https://rdrr.io/pkg/bigmemory/man/big.matrix-class.html),
the major enhancement of `hash_big_matrix` class is that the backing
files are, by default, stored in a permanent place, with the md5 of the
object as the file name. With this explicit name, `hash_big_matrix`
objects can be easily reloaded into workspace every time.

## Usage

``` r
as_hash_big_matrix(x, backingpath = "bp", silence = TRUE, ...)

attach_hash_big_matrix(x, backingpath = "bp")
```

## Arguments

- x:

  A matrix, vector, or data.frame for
  [`bigmemory::as.big.matrix()`](https://rdrr.io/pkg/bigmemory/man/big.matrix.html).

- backingpath, ...:

  Passed to
  [`bigmemory::as.big.matrix()`](https://rdrr.io/pkg/bigmemory/man/big.matrix.html).

- silence:

  Suppress messages?

## Functions

- `as_hash_big_matrix()`: Create a `hash_big_matrix` object from a
  matrix.

- `attach_hash_big_matrix()`: Attach a `hash_big_matrix` object from the
  backing file to the workspace.

## Slots

- `md5`:

  The md5 value of the matrix.

- `address`:

  Inherited from `big.matrix`.
