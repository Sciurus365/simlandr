## Changes of simlandr 0.4.1

- Standardized landscape plotting: use `autoplot()` for ggplot output and `plotly_ld()` for interactive plotly output. The previous `plot()` method
	remains available with a soft-deprecation warning. `autoplot()` is re-exported, so attaching `ggplot2` separately is not required.
- Modernized user-facing messages, warnings, and errors using `cli`.
- Used Rcpp for the Dijkstra algorithm to improve performance.
- Improved downstream portability by exporting several internal functions.
- Updated citation information.

## Test environments

- Local R installation on Windows 11 x64, R 4.6.0
- Github R-CMD-check MacOS R 4.6.0
- Github R-CMD-check Windows R 4.6.0
- Github R-CMD-check Ubuntu R 4.6.0
- Github R-CMD-check Ubuntu R-devel
- Github R-CMD-check Ubuntu R 4.5.3

## R CMD check results

0 errors | 0 warnings | 0 notes


## revdepcheck results

We checked 2 reverse dependencies (0 from CRAN + 2 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

- We saw 0 new problems
- We failed to check 0 packages

*Note from the maintainer: Above is the raw output from revdepcheck. However, the two reverse dependencies, fitlandr and Isinglandr, are from CRAN instead of Bioconductor. This may come from some problems with the revdepcheck package.*
