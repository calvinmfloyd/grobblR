# grobblR 0.2.2

## Fixed Failing Tests

* Removed test with external URL dependency that was causing CRAN check failures.
  * URL .png functionality verified locally with alternative URLs.

# grobblR 0.2.1

## Bug Fixes

* Editing the backend to solve bugs with `alter_column_names()` as a result of `tibble` moving to `3.1.4`.
* Slight speed bumps for processing text grobs.

# grobblR 0.2.0

* Adding in more error catches for `add_column_headings()`

## New Features

* Adding in `alter_column_names()` function.
  * Aiding the grouping of column names and adding in better front facing column names, without having to edit underlying column names of initial data frame.
  * Added a section into the grob matrix vignette.

# grobblR 0.1.1

## Bug fixes

* `alter_at()` now always makes aesthetic / structure changes to the correct columns when the user provides specific column names to alter.

