## Release Summary

* Fixed bug within alter_column_names() due to tibble upgrading to 3.1.3
* Sped up the process of processing strings and converting to grobs by checking if it is a valid URL structure before seeing if that URL exists

## Test environments
* local OS X install, R 4.1.1
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.