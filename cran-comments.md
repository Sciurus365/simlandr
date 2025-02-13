## Changes of simlandr 0.4.0

- Added simulation helper functions `sim_SDE()` and `multi_init_simulation()` for simulating stochastic differential equations (SDEs) and multiple initial conditions, respectively.
- Added `as.mcmc.list()` method for lists of simulation outputs. The added simulation functions also have the option to return the output as an `mcmc` or `mcmc.list` object, so that they can be used with `coda` functions.
- The landscape estimation functions now get a new parameter `weight_var` to specify the variable that contains the weight of each sample point. This may be useful if user applies importance sampling.
- Added vignettes and more examples.

## Test environments

- local R installation in Windows R 4.4.2
- Github R-CMD-check MacOS R 4.4.2
- Github R-CMD-check Windows R 4.4.2
- Github R-CMD-check Ubuntu R 4.4.2
- Github R-CMD-check Ubuntu R-devel
- Github R-CMD-check Ubuntu R 4.3.3

## R CMD check results

0 errors | 0 warnings | 1 note

Maintainer: 'Jingmeng Cui <jingmeng.cui@outlook.com>'
  
  Found the following (possibly) invalid URLs:
    URL: https://www.doi.org/10.1073/pnas.0800579105
      From: README.md
      Status: 403
      Message: Forbidden
      
I have checked the URL and it is valid. This note may come from the delayed response of the servers, or maybe it is not accessible from the checking server. It has also happened in previous submissions.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
