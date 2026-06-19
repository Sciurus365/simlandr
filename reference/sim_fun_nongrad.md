# A simple non-gradient simulation function for testing

This is a toy stochastic non-gradient system which can have
multistability in some conditions. Model specification:

## Usage

``` r
sim_fun_nongrad(
  initial = list(x1 = 0, x2 = 0, a = 1),
  parameter = list(b = 1, k = 1, S = 0.5, n = 4, lambda = 0.01, sigmasq1 = 8, sigmasq2 =
    8, sigmasq3 = 2),
  constrain_a = TRUE,
  amin = -0.3,
  amax = 1.8,
  length = 1e+05,
  stepsize = 0.01,
  seed = NULL,
  progress = TRUE
)
```

## Arguments

- initial, parameter:

  Two sets of parameters. `initial` contains the initial value of `x1`,
  `x2`, and `a`; `parameter` contains `b,k,S,n,lambda`, which control
  the model dynamics, and `sigmasq1,sigmasq2,sigmasq3`, which are the
  squares of \\\sigma_1,\sigma_2,\sigma_3\\ and controls the amplitude
  of noise.

- constrain_a:

  Should the value of `a` be constrained? (`TRUE` by default).

- amin, amax:

  If `constrain_a`, the minimum and maximum values of a.

- length:

  The length of simulation.

- stepsize:

  The step size used in the Euler method.

- seed:

  The initial seed that will be passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) function.

- progress:

  Show progress bar of the simulation?

## Value

A matrix of simulation results.

## Details

\$\$\frac {dx\_ {1}}{dt} = \frac {ax\_ {1}^ {n}}{S^ {n}+x\_ {1}^ {n}} +
\frac {bS^ {n}}{S^ {n}+x\_ {2}^ {n}} - kx\_ {1}+ \sigma_1 dW_1/dt\$\$
\$\$\frac {dx\_ {2}}{dt} = \frac {ax\_ {2}^ {n}}{S^ {n}+x\_ {2}^ {n}} +
\frac {bS^ {n}}{S^ {n}+x\_ {1}^ {n}} - kx\_ {2}+ \sigma_2 dW_2/dt\$\$
\$\$\frac {da}{dt} = -\lambda a+ \sigma_3 dW_3/dt\$\$

## References

Wang, J., Zhang, K., Xu, L., & Wang, E. (2011). Quantifying the
Waddington landscape and biological paths for development and
differentiation. Proceedings of the National Academy of Sciences,
108(20), 8257-8262.
[doi:10.1073/pnas.1017017108](https://doi.org/10.1073/pnas.1017017108)

## See also

[`sim_fun_grad()`](https://sciurus365.github.io/simlandr/reference/sim_fun_grad.md)
and
[`batch_simulation()`](https://sciurus365.github.io/simlandr/reference/batch_simulation.md).
