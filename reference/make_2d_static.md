# Make 2D static landscape plot for a single simulation output

Make 2D static landscape plot for a single simulation output

## Usage

``` r
make_2d_static(
  output,
  x,
  lims,
  kde_fun = c("ks", "base"),
  n = 200,
  h,
  adjust = 1,
  Umax = 5,
  weight_var = NULL
)

make_2d_single(
  output,
  x,
  lims,
  kde_fun = c("ks", "base"),
  n = 200,
  h,
  adjust = 1,
  Umax = 5,
  weight_var = NULL
)
```

## Arguments

- output:

  A matrix of simulation output, or a `mcmc`, `mcmc.list` object (see
  [`coda::mcmc()`](https://rdrr.io/pkg/coda/man/mcmc.html)).

- x:

  The name of the target variable.

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

- weight_var:

  The name of the weight variable, in case the weight of each
  observation is different. This may be useful when a weighted MC (e.g.,
  importance sampling) is used. Only effective for `kde_fun = "ks"`.

## Value

A `2d_static_landscape` object that describes the landscape of the
system, including the smooth distribution and the landscape plot.
