# Save landscape plots

Save landscape plots

## Usage

``` r
save_landscape(l, path = NULL, selfcontained = FALSE, ...)
```

## Arguments

- l:

  A landscape object

- path:

  The path to save the output. Default: "/pics/x_y.html".

- selfcontained:

  For 'plotly' plots, save the output as a self-contained html file?
  Default: FALSE.

- ...:

  Other parameters passed to
  [`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)
  or
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Value

The function saves the plot to a specific path. It does not have a
return value.
