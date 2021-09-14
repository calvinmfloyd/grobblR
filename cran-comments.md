## Release Summary

* Fixed bug within alter_column_names() due to tibble upgrading to 3.1.3
* Sped up the process of processing strings and converting to grobs by checking if it is a valid URL structure before seeing if that URL exists

## Test environments
* local OS X install, R 4.1.1
* GitHub Actions - (ubuntu-18.04): oldrel, release, devel
* GitHub Actions (windows): release, 3.6
* Github Actions (macOS): release, devel

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.