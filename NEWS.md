# vpc 1.2.8
- `vpc()` with `pred_corr = TRUE` and `lloq`/`uloq`: observed and simulated
  data are now handled identically. Censoring is determined on the uncorrected
  DV, only non-censored values are prediction-corrected, and censored values
  are retained as censored (instead of being dropped for simulated data) when
  calculating percentiles. Prediction-corrected observed values are no longer
  compared to the (uncorrected) limit of quantification again. The procedure
  is documented in `?vpc` (#104)

# vpc 1.2.7
- the ID column may now be character (or any non-numeric type), e.g. as
  produced by nlmixr2. Previously non-numeric IDs were coerced to `NA` when
  deriving the simulation index, causing an error (fix #103)

# vpc 1.2.6
- stratification by two variables now respects the factor level order of the
  stratification variables (fix #101)

# vpc 1.2.5
- added `pred_corr_method = c("multiplicative", "additive")` argument to `vpc()`,
  with `ltbs = TRUE` as a convenience alias for `"additive"`. The additive
  prediction-correction (`dv + (pred_bin - pred)`) is intended for
  log-transform-both-sides (LTBS) models and expects `dv` and `pred` to already
  be on the log scale. `pred_corr_lower_bnd` is ignored and `log_y = TRUE` is
  not allowed with this method (#111)

# vpc 1.2.4
- replaced defunct dplyr functions (`group_by_()`, `arrange_()`, `do()`) with
  modern equivalents (`group_by()`, `arrange()`, `slice()`) using `.data` pronoun
- fixed Rd documentation: changed `\itemize` with incorrect `\item{}` syntax to
  `\describe` with proper `\item{name}{description}` format in `new_vpc_theme()`
- removed broken `\link{sim_data}` cross-references from documentation

# vpc 1.2.3
- a major code refactoring occurred (fix #5)
- the ID column may now be a factor (fix #75)
- updates to work with ggplot2 version 3.4.0

# vpc 1.2.2
- fixes to account for changes in magrittr pipe function
- added functionality and argument to handle scales

# vpc 1.2.1
- fixes to account for dplyr 1.0.0 release

# vpc 1.2.0
- fixes to account for tibble 3.0.0 release
- fixes in calls to msg()
- fixes auto_bin S3 methods / docs
- fix in labeler function
- fix arrange() in tte parser
- add conditional=FALSE option to vpc_tte
- fix censoring bug vpc_tte
- respect factor levels in order of facets
- added extra data checks
- fix misc typos

# vpc 1.1.0
- introduce S3 method for main vpc function to support use from nlmixr
- minor fixes

# vpc 1.0.1
- minor fixes in vpc functions
- updates test to include tolerance

# vpc 1.0.0
- Initial release version
