# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

## Overview

`vpc` is a CRAN R package that builds visual predictive checks (VPCs) for pharmacometric models. It handles continuous data (`vpc()`), censored data (`vpc_cens()`), categorical data (`vpc_cat()`), and (repeated) time-to-event data (`vpc_tte()`), and it supports prediction correction (pcVPC) and reference correction (rcVPC). Docs live at vpc.ronkeizer.com, built from the `docs/` folder (gitbook, deployed with GitHub Pages).

## Commands

Run these from the package root in R:

```r
devtools::load_all()                 # load package for interactive dev
devtools::document()                 # regenerate man/ + NAMESPACE after roxygen changes
devtools::test()                     # run all tests (testthat 3rd edition)
devtools::test(filter = "ref_corr")  # run a single test file (tests/testthat/test-ref_corr.R)
devtools::check()                    # full R CMD check (what CI runs)
```

From the shell: `Rscript -e 'devtools::test(filter="vpc")'`.

CI (`.github/workflows/R-CMD-check.yaml`) runs R CMD check on macOS, Windows, and Ubuntu (devel/release/oldrel-1) on every push or PR to master.

## Rules

- Bump `Version` (and `Date`) in `DESCRIPTION` for every code change, and add an entry to `NEWS.md`.
- `man/` and `NAMESPACE` are generated. Edit the roxygen comments in `R/` and then run `devtools::document()`. Many functions share docs via `@inheritParams`, so changing a parameter's doc in one place, such as `read_vpc`, changes it in `vpc`, `vpc_cens`, and the others.
- `misc/`, `docs/`, and `.github/` are excluded from the package build (`.Rbuildignore`).

## Architecture

### Entry and dispatch
`vpc()` is an S3 generic so other packages (nlmixr, xpose) can add methods. `vpc.default()` forwards its call to `vpc_vpc()`, which holds the real continuous-data pipeline. `vpc_cens`, `vpc_cat`, and `vpc_tte` are separate top-level functions, but they reuse the same building blocks.

### Pipeline (continuous, `R/vpc.R`)
Each step is its own file in `R/`:

1. `read_vpc()` loads `sim`/`obs`/`sim_ref`, or reads them from a PsN folder (`psn_folder/m1/*.npctab`). It calls `guess_software()`, which detects `nonmem`, `phoenix`, `PKPDsim`, or `other` from the column names. The software string is prepended to the data's class. `define_data_columns()` then maps it to default column names: for example, NONMEM uses `DV/ID/TIME/PRED/NSIM`, and "other" uses lowercase `dv/id/time/pred/sim`. Users override these with `obs_cols`/`sim_cols`.
2. `define_loq()` resolves `lloq`/`uloq` into `cens_type`/`cens_limit`.
3. `format_vpc_input_data()` renames the columns to the internal standard names (`id`, `idv`, `dv`, `pred`, `sim`), applies stratification and LOQ censoring (censored values become NA), and applies the log floor. `add_sim_index_number()` builds the replicate index when the sim data has no index column.
4. Corrections:
   - **rcVPC**: `calc_ref_corr_continuous()` runs *before* binning, per record.
   - **pcVPC**: `calc_pred_corr_continuous()` runs *after* binning, because it needs the per-bin median PRED.
5. `define_bins()` / `binning.R` bin the data (default `"jenks"` via classInt; `n_bins = "auto"`).
6. `calc_vpc_continuous()` computes the observed and simulated percentiles and their CIs per bin and stratum. `quantile_cens.R` handles censoring-aware quantiles.
7. `as_vpcdb()` packs everything into a classed list (`vpcdb_continuous`, `vpcdb_categorical`, `vpcdb_censored`, `vpcdb_time_to_event`). `vpcdb = TRUE` returns this list instead of the plot.
8. `plot_vpc()` dispatches through `plot_vpc_helper.<class>` and builds the ggplot from `geom_*` helpers. What gets drawn is set by the `show` list (defaults in `show_default`, per endpoint type) and styled by `vpc_theme` (`new_vpc_theme()`, `create_vpc_theme()`).

### Correction subtleties (recent work, see NEWS 1.3.0)
- `pred_corr` and `sim_ref` cannot be combined.
- rcVPC aligns records **by position**: `obs` and every replicate in `sim` and `sim_ref` must have identical records in the same order. Sim data is censored *after* reference correction. The `log_y` floor is applied after correction, not inside `format_vpc_input_data`.
- After any correction, `lloq`/`uloq` are cleared and only NA counts as censored. Corrected values are never compared to the uncorrected limits again.
- For pcVPC, when PRED comes from the sim data, the first replicate is used, and its `id`/`idv` order must match `obs`.

### Tests
`tests/testthat/`, with one file per component. `test-ref_corr.R` checks rcVPC against anchor values computed with Pharmpy's PsN rcVPC implementation. Those values are stored in `tests/testthat/fixtures/refcorr/`, and the README there explains how to regenerate them: `make_inputs.R` produces the inputs, then `pharmpy_expected.py` runs in the `pmx:latest` Docker image. Bundled example datasets (`simple_data`, `rtte_obs_nm`, `rtte_sim_nm`) are in `data/`.

`test-plots.R` holds visual regression tests: `vdiffr::expect_doppelganger()` compares each plot with an SVG snapshot in `tests/testthat/_snaps/plots/`. Any change to plot output (including tiny theme tweaks) fails these tests. If the change is intended, inspect it with `testthat::snapshot_review("plots")` (old vs new, side by side) and accept with `testthat::snapshot_accept("plots")`, then commit the updated `.svg` files. Never accept snapshots without looking at them. A `ggplot2` upgrade can also change the SVGs; re-check and re-accept in that case. vdiffr skips these tests on CRAN.
