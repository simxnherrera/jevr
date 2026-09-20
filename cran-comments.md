## R CMD check results

0 errors | 0 warnings | 0 notes

* This GitHub release contains version 0.1.2; it has not been submitted to
  CRAN.
* Local `devtools::check(error_on = "warning")` passed with R 4.6.1 on macOS.
* GitHub Actions checks passed on Windows, macOS, Ubuntu release, Ubuntu
  devel, and Ubuntu oldrel-1.

## External service validation

The OpenRouter provider was validated with a live batched request covering
Choice, Score, and Noul. Direct live TypeSafe validation was not possible
because the maintainer account is not currently on TypeSafe's API whitelist;
the direct provider is covered by mocked transport and response tests.

## Method references

The package provides a native client for the documented TypeSafe System One
API and OpenRouter Decisions endpoint. It does not implement a new statistical
method.
