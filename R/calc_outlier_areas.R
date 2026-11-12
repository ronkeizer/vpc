#' Calculate areas where observed percentiles fall outside the simulated CI
#'
#' For each percentile shown in the VPC (lower PI bound, median, upper PI
#' bound), find the regions where the observed percentile lies outside the
#' confidence interval of the corresponding simulated percentile (the "red
#' areas" in Monolix VPCs). The returned area spans the gap between the
#' observed percentile and the nearest edge of the simulated CI.
#'
#' @param vpc_dat simulated statistics (\code{vpc_dat} element of a vpcdb)
#' @param aggr_obs observed statistics (\code{aggr_obs} element of a vpcdb)
#' @param smooth If \code{TRUE}, areas follow the lines connecting bin midpoints
#'   (with exact crossing points). If \code{FALSE}, one rectangle per bin is
#'   returned.
#' @param log_x,log_y Is the axis shown on log scale? Used so that the
#'   linear interpolation (and crossing points) matches the plotted lines.
#' @return For \code{smooth = TRUE}, a data.frame with columns \code{x}, \code{ymin}, \code{ymax},
#'   \code{group}, plus any stratification columns. For \code{smooth = FALSE}, columns
#'   \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}, \code{group} plus stratification columns.
#'   \code{NULL} when there is nothing to show.
#' @keywords internal
calc_outlier_areas <- function(vpc_dat, aggr_obs, smooth = TRUE, log_x = FALSE, log_y = FALSE) {
  if (is.null(vpc_dat) || is.null(aggr_obs)) return(NULL)
  if (is.factor(vpc_dat$bin_mid) || is.factor(aggr_obs$bin_mid)) return(NULL)
  stats_cols <- c("bin", "bin_mid", "bin_min", "bin_max",
                  "obs5", "obs50", "obs95")
  keys <- setdiff(intersect(colnames(aggr_obs), colnames(vpc_dat)), stats_cols)
  pairs <- list(q5 = "obs5", q50 = "obs50", q95 = "obs95")
  pairs <- pairs[vapply(names(pairs), function(q) {
    all(paste0(q, c(".low", ".up")) %in% colnames(vpc_dat)) && pairs[[q]] %in% colnames(aggr_obs)
  }, logical(1))]
  if (length(pairs) == 0) return(NULL)

  fx <- if (log_x) log10 else identity
  fy <- if (log_y) log10 else identity
  ifx <- if (log_x) function(x) 10^x else identity
  ify <- if (log_y) function(y) 10^y else identity

  key_str <- function(d) {
    if (length(keys) == 0) return(rep("all", nrow(d)))
    do.call(paste, c(lapply(keys, function(k) as.character(d[[k]])), sep = "\r"))
  }
  vpc_dat <- as.data.frame(vpc_dat)
  aggr_obs <- as.data.frame(aggr_obs)
  sim_key <- key_str(vpc_dat)
  obs_key <- key_str(aggr_obs)

  out <- list()
  for (k in unique(obs_key)) {
    sim_k <- vpc_dat[sim_key == k, , drop = FALSE]
    obs_k <- aggr_obs[obs_key == k, , drop = FALSE]
    if (nrow(sim_k) == 0 || nrow(obs_k) == 0) next
    key_vals <- obs_k[1, keys, drop = FALSE]
    for (q in names(pairs)) {
      if (smooth) {
        res <- outlier_areas_smooth(
          x_sim = fx(sim_k$bin_mid),
          low = fy(sim_k[[paste0(q, ".low")]]),
          up = fy(sim_k[[paste0(q, ".up")]]),
          x_obs = fx(obs_k$bin_mid),
          obs = fy(obs_k[[pairs[[q]]]])
        )
        if (is.null(res)) next
        res$x <- ifx(res$x)
        res$ymin <- ify(res$ymin)
        res$ymax <- ify(res$ymax)
      } else {
        res <- outlier_areas_bins(sim_k, obs_k, q, pairs[[q]])
        if (is.null(res)) next
      }
      res$group <- paste(k, q, res$group, sep = "_")
      if (length(keys) > 0) {
        res <- cbind(res, key_vals[rep(1, nrow(res)), , drop = FALSE], row.names = NULL)
      }
      out[[length(out) + 1]] <- res
    }
  }
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

#' @describeIn calc_outlier_areas One rectangle per bin where the observed
#'   percentile is outside the simulated CI.
#' @param sim_k,obs_k simulated/observed statistics for a single stratum
#' @param q,obs_col column prefix for the simulated percentile (e.g. "q5") and
#'   name of the observed percentile column (e.g. "obs5")
outlier_areas_bins <- function(sim_k, obs_k, q, obs_col) {
  idx <- match(obs_k$bin, sim_k$bin)
  ok <- !is.na(idx)
  if (!any(ok)) return(NULL)
  obs <- obs_k[[obs_col]][ok]
  low <- sim_k[[paste0(q, ".low")]][idx[ok]]
  up <- sim_k[[paste0(q, ".up")]][idx[ok]]
  above <- !is.na(obs) & !is.na(up) & obs > up
  below <- !is.na(obs) & !is.na(low) & obs < low
  ymin <- ifelse(above, up, ifelse(below, obs, NA))
  ymax <- ifelse(above, obs, ifelse(below, low, NA))
  res <- data.frame(
    xmin = sim_k$bin_min[idx[ok]],
    xmax = sim_k$bin_max[idx[ok]],
    ymin = ymin,
    ymax = ymax,
    group = ifelse(above, "above", "below"),
    stringsAsFactors = FALSE
  )
  res <- res[above | below, , drop = FALSE]
  if (nrow(res) == 0) return(NULL)
  res
}

#' @describeIn calc_outlier_areas Areas following the lines that connect the
#'   bin midpoints, with exact crossing points (all inputs on the plotting
#'   scale).
#' @param x_sim,low,up bin midpoints and lower/upper CI of the simulated
#'   percentile
#' @param x_obs,obs bin midpoints and value of the observed percentile
outlier_areas_smooth <- function(x_sim, low, up, x_obs, obs) {
  # Interpolate everything on a common grid. Outside the overlapping x-range,
  # or next to missing values, nothing is drawn.
  interp <- function(x, y, xout) {
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 2) return(rep(NA_real_, length(xout)))
    stats::approx(x[ok], y[ok], xout = xout, rule = 1, ties = mean)$y
  }
  # Linear interpolation that yields NA in segments with a missing endpoint,
  # so gaps (e.g. censored percentiles) are not bridged.
  interp_na <- function(x, y, xout) {
    o <- order(x)
    x <- x[o]
    y <- y[o]
    res <- interp(x, y, xout)
    fin <- is.finite(y)
    if (all(fin)) return(res)
    i <- findInterval(xout, x, rightmost.closed = TRUE)
    i[i == 0] <- 1
    j <- pmin(i + 1, length(x))
    exact <- xout == x[i]
    bad <- ifelse(exact, !fin[i], !fin[i] | !fin[j])
    res[bad] <- NA
    res
  }
  grid <- sort(unique(c(x_sim, x_obs)))
  grid <- grid[is.finite(grid)]
  if (length(grid) < 2) return(NULL)
  o <- interp_na(x_obs, obs, grid)
  l <- interp_na(x_sim, low, grid)
  u <- interp_na(x_sim, up, grid)

  one_side <- function(diff_fun, side) {
    d <- diff_fun(o, l, u)
    # Insert exact crossing points (where the difference changes sign)
    x_new <- grid
    for (i in seq_len(length(grid) - 1)) {
      if (is.na(d[i]) || is.na(d[i + 1])) next
      if (d[i] * d[i + 1] < 0) {
        x_new <- c(x_new, grid[i] + (grid[i + 1] - grid[i]) * d[i] / (d[i] - d[i + 1]))
      }
    }
    x_new <- sort(unique(x_new))
    oo <- interp_na(grid, o, x_new)
    ll <- interp_na(grid, l, x_new)
    uu <- interp_na(grid, u, x_new)
    edge <- if (side == "above") uu else ll
    dd <- diff_fun(oo, ll, uu)
    # Points where the observed percentile is outside the CI (or exactly on
    # its edge), and that are not separated by missing values.
    inside <- !is.na(dd) & dd >= -1e-9 * max(1, abs(edge), na.rm = TRUE)
    if (!any(inside)) return(NULL)
    run <- cumsum(c(TRUE, diff(inside) != 0))
    keep <- inside
    res <- data.frame(
      x = x_new[keep],
      ymin = if (side == "above") edge[keep] else oo[keep],
      ymax = if (side == "above") oo[keep] else edge[keep],
      group = paste(side, run[keep], sep = "_"),
      stringsAsFactors = FALSE
    )
    # A region needs at least two points (or a non-zero height) to be visible
    n_per <- table(res$group)
    res <- res[res$group %in% names(n_per)[n_per >= 2], , drop = FALSE]
    # Drop regions with zero area (observed exactly touching the CI edge)
    has_area <- tapply(res$ymax - res$ymin, res$group, function(h) any(h > 0))
    res <- res[res$group %in% names(has_area)[has_area], , drop = FALSE]
    if (nrow(res) == 0) return(NULL)
    res
  }
  res <- rbind(
    one_side(function(o, l, u) o - u, "above"),
    one_side(function(o, l, u) l - o, "below")
  )
  if (is.null(res) || nrow(res) == 0) return(NULL)
  res
}
