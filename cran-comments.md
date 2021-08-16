* This is the first CRAN release for a new package.

## Resubmission
This is a resubmission. In this version I have:

* Added the reference of the method in the DESCRIPTION.
* Checked the functions and converted all `T` and `F` to `TRUE` and `FALSE`.
* Added the `\value` section for all function documents.

## Test environments

* local R installation in Windows, R 4.0.5
* windows-latest (release, on Github R-CMD-Check), R 4.1.0
* macOS-latest (release, on Github R-CMD-Check), R 4.1.0
* ubuntu-20.04 (release, on Github R-CMD-Check), R 4.1.0
* CRAN win-builder, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

There was 1 NOTE:

* installed size is  6.3Mb
  sub-directories of 1Mb or more:
        doc   5.7Mb
  This is because the vignettes contain several plots. Because producing pictures is a very important utility of this package, it is not feasible to delete the plots.
