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
- fixes for dplyr 1.0.0

## Downstream dependencies
I have also run R CMD check on downstream dependencies of vpc
All packages that I could install passed except:

* xpose: I've put in a PR for that package to fix the reference
object that makes the test fail.