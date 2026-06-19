# A simple gradient simulation function for testing

This is a toy stochastic gradient system which can have bistability in
some conditions. Model specification: \$\$U = x^4 + y^4 + axy + bx +
cy\$\$ \$\$dx/dt = - \partial U/ \partial x + \sigma dW/dt = - 4x^3 -
ay - b + \sigma dW/dt\$\$ \$\$dy/dt = - \partial U/ \partial y + \sigma
dW/dt = - 4y^3 - ax - c + \sigma dW/dt\$\$

## Usage

``` r
sim_fun_grad(
  initial = list(x = 0, y = 0),
  parameter = list(a = -4, b = 0, c = 0, sigmasq = 1),
  length = 1e+05,
  stepsize = 0.01,
  seed = NULL
)
```

## Arguments

- initial, parameter:

  Two sets of parameters. `initial` contains the initial value of `x`
  and `y`; `parameter` contains `a,b,c`, which control the shape of the
  potential landscape, and `sigmasq`, which is the square of \\\sigma\\
  and controls the amplitude of noise.

- length:

  The length of simulation.

- stepsize:

  The step size used in the Euler method.

- seed:

  The initial seed that will be passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) function.

## Value

A matrix of simulation results.

## See also

[`sim_fun_nongrad()`](https://sciurus365.github.io/simlandr/reference/sim_fun_nongrad.md)
and
[`batch_simulation()`](https://sciurus365.github.io/simlandr/reference/batch_simulation.md).
