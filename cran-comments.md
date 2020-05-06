## Test environments
* local OS X install, R 3.6.3
* ubuntu 20.04, R 4.0.0
* win-builder, R Under development (unstable) (2020-05-05 r78369)

## R CMD check results
R CMD check results
0 errors | 0 warnings | 0 notes

R CMD check succeeded

## Downstream dependencies
xpose, nlmixr, xpose.nlmixr, tidyvpc

## Current release notes:
- fixes for tibble 3.0.0
- fixes in calls to msg()
- fixes auto_bin S3 methods / docs
- fix in labeler function
- fix arrange() in tte parser
- add conditional=FALSE option to vpc_tte
- fix censoring bug vpc_tte
- respect factor levels in order of facets
- added extra data checks
- fix misc typos