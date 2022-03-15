## Changes of simlandr 0.2.0

* Renamed `var` and `par` in batch simulation functions to `arg` (argument) and `ele` (element).
* Adjusted the algorithm of calculating minimal energy paths (previously "minimal elevation paths").
* Added an nongradient example function `sim_fun_nongrad()`; renamed `sim_fun_test2()` to `sim_fun_grad()`.
* Added title for the color bar in `plot_ly` based plots; used `theme_bw()` throughout all `ggplot2` based plots.
* Improved some documentations; included a new vignette.

## Test environments

* local R installation in Windows, R 4.1.2
* CRAN macOS builder, R 4.1.1
* CRAN win-builder, R 4.1.3
* CRAN win-builder, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

There was one NOTE:

Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.1073/pnas.1017017108
      From: man/sim_fun_nongrad.Rd
      Status: 503
      Message: Service Unavailable
  
Found the following (possibly) invalid DOIs:
    DOI: 10.1073/pnas.0800579105
      From: DESCRIPTION
      Status: Service Unavailable
      Message: 503

I have checked those two DOIs and they are valid. The second DOI is also the same as the previous version of this package but last time this information was not shown. These should be false positives.
