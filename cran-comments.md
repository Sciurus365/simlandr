## Changes of simlandr 0.3.1

- Removed unused parameter `Umax` from the documentation of `make_kernel_dist()` (an internal function). 

## Test environments

-   local R installation in Windows R 4.3.2
-   Github R-CMD-check MacOS R 4.3.2
-   Github R-CMD-check Windows R 4.3.2
-   Github R-CMD-check Ubuntu R 4.3.2
-   Github R-CMD-check Ubuntu R-devel
-   Github R-CMD-check Ubuntu R 4.2.3

## R CMD check results

0 errors | 0 warnings | 1 note

Maintainer: 'Jingmeng Cui <jingmeng.cui@outlook.com>'
  
  Found the following (possibly) invalid URLs:
    URL: https://psyarxiv.com/pzva3/ (moved to https://osf.io/preprints/psyarxiv/pzva3/)
      From: README.md
      Status: 200
      Message: OK
    URL: https://www.doi.org/10.1073/pnas.0800579105
      From: README.md
      Status: 403
      Message: Forbidden
      
I have checked the URLs and they are all valid. Those notes may come from the delayed response of the servers, or maybe they are not accessible from the checking server.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
