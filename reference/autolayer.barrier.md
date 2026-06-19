# Get a ggplot2 layer from a barrier object

This layer can show the saddle point (2d) and the minimal energy path
(3d) on the landscape.

## Usage

``` r
# S3 method for class 'barrier'
autolayer(object, path = TRUE, ...)
```

## Arguments

- object:

  A `barrier` object.

- path:

  Show the minimum energy path in the graph?

- ...:

  Not in use.

## Value

A `ggplot2` layer that can be added to an existing landscape plot.
