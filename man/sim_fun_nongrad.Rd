% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_function.R
\name{sim_fun_nongrad}
\alias{sim_fun_nongrad}
\title{A simple non-gradient simulation function for testing}
\usage{
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
}
\arguments{
\item{initial, parameter}{Two sets of parameters. \code{initial} contains the initial value of \code{x1}, \code{x2}, and \code{a};
\code{parameter} contains \verb{b,k,S,n,lambda}, which control the model dynamics,
and \verb{sigmasq1,sigmasq2,sigmasq3}, which are the squares of \eqn{\sigma_1,\sigma_2,\sigma_3} and controls the amplitude of noise.}

\item{constrain_a}{Should the value of \code{a} be constrained? (\code{TRUE} by default).}

\item{amin, amax}{If \code{constrain_a}, the minimum and maximum values of a.}

\item{length}{The length of simulation.}

\item{stepsize}{The step size used in the Euler method.}

\item{seed}{The initial seed that will be passed to \code{set.seed()} function.}

\item{progress}{Show progress bar of the simulation?}
}
\value{
A matrix of simulation results.
}
\description{
This is a toy stochastic non-gradient system which can have multistability in some conditions.
Model specification:
}
\details{
\deqn{\frac {dx_ {1}}{dt}  =  \frac {ax_ {1}^ {n}}{S^ {n}+x_ {1}^ {n}} + \frac {bS^ {n}}{S^ {n}+x_ {2}^ {n}} - kx_ {1}+ \sigma_1 dW_1/dt}
\deqn{\frac {dx_ {2}}{dt}  =  \frac {ax_ {2}^ {n}}{S^ {n}+x_ {2}^ {n}} + \frac {bS^ {n}}{S^ {n}+x_ {1}^ {n}} - kx_ {2}+ \sigma_2 dW_2/dt}
\deqn{\frac {da}{dt} = -\lambda a+ \sigma_3 dW_3/dt}
}
\references{
Wang, J., Zhang, K., Xu, L., & Wang, E. (2011). Quantifying the Waddington landscape and biological paths for development and differentiation. Proceedings of the National Academy of Sciences, 108(20), 8257-8262.
\doi{10.1073/pnas.1017017108}
}
\seealso{
\code{\link[=sim_fun_grad]{sim_fun_grad()}} and \code{\link[=batch_simulation]{batch_simulation()}}.
}
