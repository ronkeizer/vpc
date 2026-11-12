# vpc 1.3.5
- Bins can now be determined per stratum (#11). With `bin_by_strata = TRUE` in
  `vpc()` and `vpc_cens()`, automatic binning methods are applied to the data
  of each stratum separately. Manual bins can be specified per stratum by
  passing a list to `bins` (named by stratum, or unnamed in the order of the
  strata), e.g. `bins = list(A = c(0, 2, 6, 24), B = c(0, 4, 12, 48))`, which
  can also mix binning methods and numeric separators. `n_bins` can likewise be
  given per stratum. The bin separators shown in the plot are drawn per panel.
  With per-stratum binning, `db$bins` in the vpcdb object is a named list.
  Missing values of the stratification variable are binned as their own
  stratum (with an `NA` name in `db$bins`; distinct from a stratum with the
  literal value `"NA"`). Not available for `bins = "percentiles"`.
- With two stratification variables, missing values are now labeled `"<NA>"`
  in the combined stratum (e.g. `"<NA>, X"`), so they are no longer merged
  with the literal value `"NA"` (`"NA, X"`) when calculating the VPC. Commas
  and backslashes within values are escaped in the combined stratum, so e.g.
  `("a, b", "c")` and `("a", "b, c")` are no longer merged.
- `vpc_cens()` with `stratify_color`: the color stratification is now
  implemented. The observed fraction and simulated CI are calculated per
  color stratum and drawn in separate colors (with legend), while facets are
  determined by `stratify` only. Previously the color variable was added only
  after the data were stratified, so it was ignored (color only) or
  mislabeled as a facet (facet and color).

# vpc 1.3.4
- Continuous VPCs: the observed lower and upper percentile lines are now drawn
  as separate lines instead of as the outline of one ribbon. Before, when one
  observed percentile was censored (`NA`, e.g. below `lloq`) in a bin, the
  other percentile was not drawn in that bin either (#122)

# vpc 1.3.3
- new `show = list(outlier_areas = TRUE)` for continuous VPCs: highlights
  (in red) the areas where an observed percentile falls outside the
  confidence interval of the corresponding simulated percentile, as in
  Monolix VPCs. Also available in `vpc_cens()` (fraction below/above LOQ)
  and `vpc_cat()` (fraction per category). Works with `smooth = TRUE/FALSE`,
  stratification, and log axes. Color and transparency are set with the new
  `outlier_fill` and `outlier_alpha` elements of `new_vpc_theme()` (#8)

# vpc 1.3.2
- Added visual regression tests (#9): `tests/testthat/test-plots.R` uses
  `vdiffr` to snapshot the VPC plots (continuous, pcVPC, LLOQ/log-y,
  stratified, censored, categorical, time-to-event) as SVGs in
  `tests/testthat/_snaps/plots/`. Review intended changes side by side with
  `testthat::snapshot_review("plots")`. `vdiffr` is a new suggested package;
  the visual tests are skipped on CRAN or when it is not installed.

# vpc 1.3.1
- `auto_bin()` / `bins = "data"`: the lowest observations were not assigned to
  any bin (and thus silently excluded from the VPC). All data are now binned,
  with bins containing approximately equal numbers of observations. The
  automatic binning methods now also include the maximum value for negative
  independent variables.
- `auto_bin()` with `type = "time"` or `"data"` now returns the requested
  number of bins (previously `n_bins + 1`).
- `vpc_tte()` with `kmmc` and stratification: the KMMC curve is now computed
  per stratum, instead of showing the pooled curve in every panel. Simulated
  curves are no longer carried over between strata with different time grids.
- `pk_iv_1cmt()` now returns concentrations in the order of the supplied
  times, and `pk_oral_1cmt()` no longer returns `NaN` when `ka == ke` (incl.
  the defaults).
- removed unused internal helpers and improved test coverage (#94)

# vpc 1.3.0
- added reference-corrected VPC (rcVPC; Ibrahim et al., AAPS J 2025) via new
  `sim_ref` argument to `vpc()`. `pred_corr_method = "additive"` (or
  `ltbs = TRUE`) selects the log-scale equations, and the lower bound is taken
  from `pred_corr_lower_bnd`. Censored data is handled as for pcVPC. Validated
  against Pharmpy's PsN implementation (#105)
- pcVPC: when PRED is taken from the simulated data, the first replicate is
  used if its records (`id`, `idv`) match the observed data; otherwise a
  warning is given and the previous behaviour (first rows) is used

# vpc 1.2.9
- `vpc()` with `pred_corr = TRUE` and `lloq`/`uloq`: observed and simulated
  data are now handled identically. Censoring is determined on the uncorrected
  DV, only non-censored values are prediction-corrected, and censored values
  are retained as censored (instead of being dropped for simulated data) when
  calculating percentiles. Prediction-corrected observed values are no longer
  compared to the (uncorrected) limit of quantification again. The procedure
  is documented in `?vpc` (#104)

# vpc 1.2.8
- added `ci_sim` argument to `vpc_tte()` to specify the confidence interval
  of the Kaplan-Meier curves from the simulated data, which was previously
  fixed at 90% (fix #102). The `ci` argument only applies to the observed data.

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
