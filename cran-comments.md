## R CMD check results

0 errors | 0 warnings | 0 notes

* This GitHub release contains version 0.2.0; it has not been submitted to
  CRAN.
* Local `devtools::check(error_on = "warning")` passed with R 4.6.1 on macOS.
* GitHub Actions checks passed on Windows, macOS, Ubuntu release, Ubuntu
  devel, and Ubuntu oldrel-1.

## External service validation

The direct TypeSafe and OpenRouter System One endpoints were validated with
live requests covering Choice, Score, and Noul, multi-state evaluation,
model listing (TypeSafe), and the legacy OpenRouter Decisions mapping. The
Vercel AI Gateway and Pydantic AI Gateway presets, and Vercel decision
fallbacks, are covered by mocked tests against their documented request and
response shapes only.

## Method references

The package provides a native client for the documented TypeSafe System One
API and compatible gateways. It does not implement a new statistical method.
