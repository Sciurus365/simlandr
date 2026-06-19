# Simulate 1-3D Markovian Stochastic Differential Equations

A wrapper to the simulation utilities provided by the Sim.DiffProc
package. You may skip this step and write your own simulation function
for more customized simulation.

## Usage

``` r
sim_SDE(
  N = 1000,
  M = 1,
  x0,
  t0 = 0,
  T = 1,
  Dt = rlang::missing_arg(),
  drift,
  diffusion,
  corr = NULL,
  alpha = 0.5,
  mu = 0.5,
  type = "ito",
  method = "euler",
  keep_full = TRUE
)
```

## Arguments

- N:

  The number of time steps.

- M:

  The number of simulations.

- x0:

  The initial values of the SDE. The number of values determine the
  dimension of the SDE.

- t0:

  initial time.

- T:

  terminal time.

- Dt:

  time step. If missing, default will be (T - t0) / N.

- drift:

  An expression of the drift function. The number of expressions
  determine the dimension of the SDE. Should be the function of `t`,
  `x`, `y` and `z` (`y` and `z` are only included for 2D or 3D cases).

- diffusion:

  An expression of the diffusion function. The number of expressions
  determine the dimension of the SDE. Should be the function of `t`,
  `x`, `y` and `z` (`y` and `z` are only included for 2D or 3D cases).

- corr:

  The correlations between the Brownian motions. Only used for 2D or 3D
  cases. Must be a real symmetric positive-definite matrix of size 2x2
  or 3x3. If NULL, the default is the identity matrix.

- alpha, mu:

  weight of the predictor-corrector scheme; the default `alpha = 0.5`
  and `mu = 0.5`.

- type:

  if `type="ito"` simulation sde of Itô type, else `type="str"`
  simulation sde of Stratonovich type; the default `type="ito"`.

- method:

  numerical methods of simulation, the default `method = "euler"`.

- keep_full:

  Whether to keep the full snssde1d/snssde2d/snssde3d object. If TRUE,
  the full object will be returned. If FALSE, only the simulated values
  will be returned as a matrix or a list of matrices (when `M >= 2`).

## Value

Depending on the value of `keep_full`, the output will be a list of
`snssde1d`, `snssde2d` or `snssde3d` objects, or a matrix or a list of
matrices of the simulated values.

## Examples

``` r
# From the Sim.DiffProc package

set.seed(1234, kind = "L'Ecuyer-CMRG")
mu <- 4
sigma <- 0.1
fx <- expression(y, (mu * (1 - x^2) * y - x))
gx <- expression(0, 2 * sigma)
mod2d <- sim_SDE(drift = fx, diffusion = gx, N = 1000,
Dt = 0.01, x0 = c(0, 0), type = "str", method = "rk1",
M = 2, keep_full = FALSE)
#> Error in eval(drifty): object 'mu' not found

print(as.mcmc.list(mod2d))
#> Error: object 'mod2d' not found
```
