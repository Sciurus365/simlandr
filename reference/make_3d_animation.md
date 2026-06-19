# Make 3d animations from multiple simulations

Make 3d animations from multiple simulations

## Usage

``` r
make_3d_animation(
  bs,
  x,
  y,
  fr,
  lims,
  kde_fun = c("ks", "MASS"),
  n = 200,
  h,
  adjust = 1,
  Umax = 5,
  individual_landscape = TRUE,
  mat_3d = FALSE
)
```

## Arguments

- bs:

  A `batch_simulation` object created by `[batch_simulation()].`

- x, y:

  The names of the target variables.

- fr:

  The names of the parameters used to represent frames in the animation.

- lims:

  The limits of the range for the density estimator as `c(xl, xu)` for
  2D landscapes, `c(xl, xu, yl, yu)` for 3D landscapes,
  `c(xl, xu, yl, yu, zl, zu)` for 4D landscapes. If missing, the range
  of the data extended by 10% for both sides will be used. For
  landscapes based on multiple simulations, the largest range of all
  simulations (which means the lowest lower limit and the highest upper
  limit) will be used by default.

- kde_fun:

  Which kernel estimator to use? Choices: "ks"
  [`ks::kde()`](https://mvstat.net/ks/reference/kde.html) (default;
  faster and using less memory); "base" `base::density()` (only for 2D
  landscapes); "MASS"
  [`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html) (only for
  3D landscapes).

- n:

  The number of equally spaced points in each axis, at which the density
  is to be estimated.

- h:

  A number, or possibly a vector for 3D and 4D landscapes, specifying
  the smoothing bandwidth to be used. If missing, the default value of
  the kernel estimator will be used (but `bw = "SJ"` for
  `base::density()`). Note that the definition of bandwidth might be
  different for different kernel estimators. For landscapes based on
  multiple simulations, the largest `h` of all simulations will be used
  by default.

- adjust:

  The multiplier to the bandwidth. The bandwidth used is actually
  `adjust * h`. This makes it easy to specify values like "half the
  default" bandwidth.

- Umax:

  The maximum displayed value of potential.

- individual_landscape:

  Make individual landscape for each simulation? Default is `TRUE` so
  that it is possible to calculate barriers. Set to `FALSE` to save
  time.

- mat_3d:

  Also make the matrix by
  [`make_3d_matrix()`](https://sciurus365.github.io/simlandr/reference/make_3d_matrix.md)?
  If so, the matrix can be drawn with
  `ggplot2::autoplot(<landscape>$mat_3d)`.

## Value

A `3d_animation_landscape` object that describes the landscape of the
system, including the smoothed distribution and the landscape plot.
