## R CMD check results

0 errors | 0 warnings | 0 notes

## Summary

This release fixes CRAN check errors caused by defunct dplyr functions:

- Replaced `group_by_()`, `arrange_()`, and `do()` with modern dplyr equivalents
  (`group_by()`, `arrange()`, `slice()`) using the `.data` pronoun from rlang
- Fixed Rd documentation formatting issues ("Lost braces" warnings) in
  `new_vpc_theme.Rd`
- Removed broken `\link{sim_data}` cross-references from documentation

## Test environments

- local macOS (aarch64-apple-darwin20), R 4.4.2
- GitHub Actions (ubuntu-latest), R release
- GitHub Actions (ubuntu-latest), R devel

## Downstream dependencies

There are no known reverse dependencies on CRAN.
