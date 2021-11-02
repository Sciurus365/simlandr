## Test environments

* local R installation in Windows, R 4.1.1
* windows-latest (release, on Github R-CMD-Check), R 4.1.1
* macOS-latest (release, on Github R-CMD-Check), R 4.1.1
* ubuntu-20.04 (release, on Github R-CMD-Check), R 4.1.1
* CRAN win-builder, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

There was 1 NOTE:

* installed size is  6.4Mb
  sub-directories of 1Mb or more:
        doc   5.8Mb
  This is because the vignettes contain several plots. Because producing pictures is a very important utility of this package, it is not feasible to delete the plots.
