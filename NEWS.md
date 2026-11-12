# vpc 1.3.0
- added reference-corrected VPC (rcVPC; Ibrahim et al., AAPS J 2025) via new
  `sim_ref` argument to `vpc()`, with `ref_corr_log` for log-scale DV. The lower
  bound is taken from `pred_corr_lower_bnd`. Validated against Pharmpy's PsN
  implementation (#105)
- pcVPC with `lloq`/`uloq`: pred-corrected observations are no longer compared
  to the uncorrected limit again when calculating observed percentiles; only
  values censored before correction are treated as censored
- pcVPC: when PRED is taken from the simulated data, the first replicate is
  used and its record order (`id`, `idv`) must match the observed data

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
