# Plot landscape objects

**\[deprecated\]**

## Usage

``` r
# S3 method for class 'landscape'
plot(x, index = 1, ...)
```

## Arguments

- x:

  A landscape object

- index:

  Default is 1. For some landscape objects, there is a second plot
  (usually 2d heatmaps for 3d landscapes) or a third plot (usually 3d
  matrices for 3d animations). Use `index = 2` to plot that one.

- ...:

  Arguments passed to the replacement method.

## Value

The plot.

## Details

Use
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
for ggplot output or
[`plotly_ld()`](https://sciurus365.github.io/simlandr/reference/plotly_ld.md)
for interactive landscape output.
