## Changes of simlandr 0.2.1

* Update roxygen2 version to ensure html5 compatibility (>= R 4.2.0).

## Test environments

* local R installation in Windows, R 4.2.1
* CRAN win-builder, R 4.2.1
* CRAN win-builder, R-devel
* R-hub Ubuntu Linux 20.04.1 LTS, R 4.2.1, GCC

## R CMD check results

0 errors | 0 warnings | 1 note

There was one NOTE:

Found the following (possibly) invalid URLs:
  URL: https://www.doi.org/10.1073/pnas.0800579105
    From: README.md
    Status: 503
    Message: Service Unavailable

Found the following (possibly) invalid DOIs:
  DOI: 10.1073/pnas.0800579105
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503

I have checked those DOIs and they are valid, so I believe this note is a false positive.
