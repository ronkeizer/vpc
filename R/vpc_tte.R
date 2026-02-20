#' VPC function for time-to-event (survival) data
#'
#' This function can be used for either single time-to-event (TTE) or repeated time-to-event (RTTE) data.
#'
#' Creates a VPC plot from observed and simulation survival data
#'
#' @inheritParams format_vpc_input_data
#' @inheritParams read_vpc
#' @inheritParams plot_vpc
#' @inheritParams as_vpcdb
#' @inheritParams define_bins
#' @inheritParams compute_kaplan
#' @param kmmc either NULL (for regular TTE vpc, default), or a variable name for a KMMC plot (e.g. "WT")
#' @param events numeric vector describing which events to show a VPC for when repeated TTE data, e.g. c(1:4). Default is NULL, which shows all events.
#' @param reverse_prob reverse the probability scale (i.e. plot 1-probability)
#' @param stratify_color character vector of stratification variables. Only 1 stratification variable can be supplied, cannot be used in conjunction with `stratify`.
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param as_percentage Show y-scale from 0-100 percent? TRUE by default, if FALSE then scale from 0-1.
#' @param verbose show debugging information (TRUE or FALSE)
#' @param vpcdb Boolean whether to return the underlying vpcdb rather than the plot
#' @return a list containing calculated VPC information (when vpcdb=TRUE), or a ggplot2 object (default)
#' @export
#' @seealso \link{vpc}, \link{vpc_tte}, \link{vpc_cens}
#' @examples
#' ## See vpc-docs.ronkeizer.com for more documentation and examples.
#'
#' ## Example for repeated) time-to-event data
#' ## with NONMEM-like data (e.g. simulated using a dense grid)
#'
#' data(rtte_obs_nm)
#' data(rtte_sim_nm)
#'
#' # treat RTTE as TTE, no stratification
#' vpc_tte(sim = rtte_sim_nm[rtte_sim_nm$sim <= 20,],
#'        obs = rtte_obs_nm,
#'        rtte = FALSE,
#'        sim_cols=list(dv = "dv", idv = "t"), obs_cols=list(idv = "t"))
#'
vpc_tte <- function(sim = NULL,
                    obs = NULL,
                    psn_folder = NULL,
                    rtte = FALSE,
                    rtte_calc_diff = TRUE,
                    rtte_conditional = TRUE,
                    events = NULL,
                    bins = FALSE,
                    n_bins = 10,
                    software = "auto",
                    obs_cols = NULL,
                    sim_cols = NULL,
                    kmmc = NULL,
                    reverse_prob = FALSE,
                    stratify = NULL,
                    stratify_color = NULL,
                    ci = c(0.05, 0.95),
                    xlab = "Time",
                    ylab = "Survival (%)",
                    show = NULL,
                    as_percentage = TRUE,
                    title = NULL,
                    smooth = FALSE,
                    vpc_theme = NULL,
                    facet = "wrap",
                    labeller = NULL,
                    verbose = FALSE,
                    vpcdb = FALSE) {
  vpc_data <- read_vpc(
    sim=sim, obs=obs, psn_folder=psn_folder,
    software=software,
    sim_cols=sim_cols, obs_cols=obs_cols
  )
  sim <- vpc_data$sim
  obs <- vpc_data$obs
  software_type <- vpc_data$software
  cols <- vpc_data$cols
  if(!is.null(kmmc)) {
    if(!kmmc %in% names(obs)) {
      stop(paste0("Specified covariate ", kmmc, " not found among column names in observed data."))
    }
  }
  if(!is.null(kmmc)) {
    if(!kmmc %in% names(sim)) {
      stop(paste0("Specified covariate ", kmmc, " not found among column names in simulated data."))
    }
  }
  msg("Initializing...", verbose=verbose)

  ## checking whether stratification columns are available
  msg("Stratifying data...", verbose=verbose)
  stratify_pars <-
    if (!is.null(stratify) & !is.null(stratify_color)) {
      stop("Sorry, stratification using both facets and color is currently not supported, use either `stratify` or `stratify_color`.")
    } else if (!is.null(stratify)) {
      stratify
    } else if (!is.null(stratify_color)) {
      if(length(stratify_color) != 1) {
        stop("Sorry, please specify only a single stratification variable for `stratify_color`.")
      }
      stratify_color
    } else {
      NULL
    }
  check_stratification_columns_available(data=obs, stratify=stratify_pars, type="observation")
  check_stratification_columns_available(data=sim, stratify=stratify_pars, type="simulation")

  ## redefine strat column in case of "strat"
  if(!is.null(stratify_pars) && !is.null(obs)) {
    if(stratify_pars[1] == "strat") {
      if(!is.null(obs)) {
        obs$strat_orig = obs$strat
      } else if (!is.null(sim)){
        sim$strat_orig = sim$strat
      }
      stratify <- "strat_orig"
    }
  }

  ## stratification
  stratify_original <- stratify_pars
  if(!is.null(stratify_pars)) {
    if(rtte) {
      if (length(stratify_pars) > 1) {
        stop ("Sorry, with repeated time-to-event data, stratification on more than 1 variables is currently not supported!")
        invisible()
      }
    } else {
      if (length(stratify_pars) > 2) {
        stop ("Sorry, stratification on more than 2 variables is currently not supported!")
        invisible()
      }
    }
  }

  ## format obs data
  if(!is.null(obs)) {
    obs_data <-
      format_vpc_input_data_tte(
        dat=obs,
        cols=vpc_data$cols$obs,
        stratify=stratify_pars,
        rtte=rtte,
        rtte_calc_diff=rtte_calc_diff,
        what="observed",
        verbose=verbose
      )
    obs <- obs_data$dat
    stratify_pars <- obs_data$stratify

    # add stratification column and compute KM curve for observations
    if(!is.null(kmmc) && kmmc %in% names(obs)) {
      obs_km <- compute_kmmc(obs, strat = "strat", reverse_prob = reverse_prob, kmmc=kmmc)
    } else {
      # Obs data is still used to calculate bins, even though obs data not
      # requested in plot
      obs_km <- compute_kaplan(obs, strat = "strat", reverse_prob = reverse_prob, rtte_conditional = rtte_conditional, ci = ci)
    }
  } else { # get bins from sim
    obs_km <- NULL
  }

  all_dat <- c()
  if(!is.null(sim)) {
    sim_data <-
      format_vpc_input_data_tte(
        dat=sim,
        cols=vpc_data$cols$sim,
        stratify=stratify_pars,
        rtte=rtte,
        rtte_calc_diff=rtte_calc_diff,
        what="simulated",
        verbose=verbose
      )
    sim <- sim_data$dat

    n_sim <- length(unique(sim$sim))
    if(n_sim <= 1) {
      stop(paste0("Something seems wrong with your simulation dataset, only ", n_sim, " iterations of the simulation were identified."))
    }
    bins_data <- define_bins_tte(obs=obs, sim=sim, bins=bins, n_bins=n_bins, kmmc=kmmc, verbose=verbose)
    tmp_bins <- bins_data$tmp_bins
    msg("Calculating simulation stats...", verbose=verbose)
    if (verbose) pb <- utils::txtProgressBar(min = 1, max = n_sim)
    for (i in 1:n_sim) {
      if (verbose) utils::setTxtProgressBar(pb, i)
      tmp <- sim %>% dplyr::filter(sim == i)
      tmp2 <- add_stratification(tmp %>%
                                 dplyr::arrange(id, time), stratify_pars)
      if(!is.null(kmmc) && kmmc %in% names(obs)) {
        tmp3 <- compute_kmmc(tmp2, strat = "strat", reverse_prob = reverse_prob, kmmc = kmmc)
      } else {
        tmp3 <- compute_kaplan(tmp2, strat = "strat", reverse_prob = reverse_prob, rtte_conditional = rtte_conditional)
      }
      tmp3$time_strat <- paste0(tmp3$time, "_", tmp3$strat)
      tmp4 <- expand.grid(time = c(0, unique(sim$time)), surv=NA, lower=NA, upper=NA,
                          strat = unique(tmp3$strat))
      tmp4$time_strat <- paste0(tmp4$time, "_", tmp4$strat)
      tmp4[match(tmp3$time_strat, tmp4$time_strat),]$surv <- tmp3$surv
#       tmp4[match(tmp3$time_strat, tmp4$time_strat),]$lower <- tmp3$lower
#       tmp4[match(tmp3$time_strat, tmp4$time_strat),]$upper <- tmp3$upper
      tmp4 <- tmp4 %>%
        dplyr::arrange(strat, time)
      tmp4$surv <- locf(tmp4$surv)
      tmp4[,c("bin", "bin_min", "bin_max", "bin_mid")] <- 0
      tmp4$bin <- cut(tmp4$time, breaks = tmp_bins, labels = FALSE, right = TRUE)
      tmp4$bin_min <- tmp_bins[tmp4$bin]
      tmp4$bin_max <- tmp_bins[tmp4$bin+1]
      tmp4$bin_mid <- (tmp4$bin_min + tmp4$bin_max) / 2
      all_dat <- dplyr::bind_rows(all_dat, cbind(i, tmp4)) ## RK: this can be done more efficient!
    }
    if (verbose) close(pb)
    sim_km <- all_dat %>%
      dplyr::group_by(strat, bin) %>%
      dplyr::summarise (bin_mid = head(bin_mid,1),
                 bin_min = head(bin_min,1),
                 bin_max = head(bin_max,1),
                 qmin = quantile(surv, 0.05),
                 qmax = quantile(surv, 0.95),
                 qmed = median(surv),
#                         lower_med = median(lower, 0.05),
#                         upper_med = median(upper, 0.05),
                 step = 0)
  } else {
    sim_km <- NULL
    bins_data <- define_bins_tte(obs=obs, sim=sim, bins=bins, n_bins=n_bins, kmmc=kmmc, verbose=verbose)
    tmp_bins <- bins_data$tmp_bins
  }

  if (rtte) {
    if(!is.null(sim)) {
      sim_km$rtte <- as.num(gsub(".*, (\\d)", "\\1", sim_km$strat, perl = TRUE))
      if (!is.null(events)) {
        sim_km <- sim_km %>%
          dplyr::filter(rtte %in% events)
        # redefine strat factors, since otherwise empty panels will be shown
        sim_km$strat <- factor(sim_km$strat, levels = unique(sim_km$strat))
      }
    }
    if(!is.null(obs)) {
      obs_km$rtte <- as.num(gsub(".*, (\\d)", "\\1", obs_km$strat, perl = TRUE))
      if (!is.null(events)) {
        obs_km <- obs_km %>%
          dplyr::filter(rtte %in% events)
        obs_km$strat <- factor(obs_km$strat, levels = unique(obs_km$strat))
      }
    }
  }

  cens_dat <- NULL
  if(!is.null(obs)) {
    # Obs data is still used to calculate bins, even though obs data not
    # requested in plot
    cens_dat <- obs
    if(rtte) {
      if(!rtte_conditional || !rtte_calc_diff) {
        cens_dat <- cens_dat %>%
          dplyr::mutate(time = t)
      }
    }
    cens_dat <- cens_dat %>%
      dplyr::filter(dv == 0, time > 0)
  }

  if (!is.null(stratify_original)) {
    if (length(stratify_pars) == 2) {
      if(!is.null(sim_km)) {
        sim_km$strat1 <- unlist(strsplit(as.character(sim_km$strat), ", "))[(1:length(sim_km$strat)*2)-1]
        sim_km$strat2 <- unlist(strsplit(as.character(sim_km$strat), ", "))[(1:length(sim_km$strat)*2)]
      }
      if(!is.null(obs_km)) {
        obs_km$strat1 <- unlist(strsplit(as.character(obs_km$strat), ", "))[(1:length(obs_km$strat)*2)-1]
        obs_km$strat2 <- unlist(strsplit(as.character(obs_km$strat), ", "))[(1:length(obs_km$strat)*2)]
      }
      if(!is.null(cens_dat)) {
        cens_dat$strat1 <- unlist(strsplit(as.character(cens_dat$strat), ", "))[(1:length(cens_dat$strat)*2)-1]
        cens_dat$strat2 <- unlist(strsplit(as.character(cens_dat$strat), ", "))[(1:length(cens_dat$strat)*2)]
      }
    }
  }

  if (!is.null(obs)) {
    if(nrow(cens_dat)>0) {
      cens_dat$y <- 1
      cens_dat$strat1 <- NA
      cens_dat$strat2 <- NA
      for (j in 1:nrow(cens_dat[,1])) {
        tmp <- obs_km[as.character(obs_km$strat) == as.character(cens_dat$strat[j]),]
        cens_dat$y[j] <- rev(tmp$surv[(cens_dat$time[j] - tmp$time) > 0])[1]
        if ("strat1" %in% names(tmp)) {
          cens_dat$strat1[j] <- rev(tmp$strat1[(cens_dat$time[j] - tmp$time) > 0])[1]
        }
        if ("strat2" %in% names(tmp)) {
          cens_dat$strat2[j] <- rev(tmp$strat2[(cens_dat$time[j] - tmp$time) > 0])[1]
        }
      }
      cens_dat <- cens_dat[!is.na(cens_dat$y),]
    }
  }
  if(!is.null(obs)) {
    show$obs_dv <- TRUE
  } else {
    show$obs_dv <- FALSE
  }
  show$pi <- TRUE
  if(!is.null(kmmc)) {
    ylab <- paste0("Mean (", kmmc, ")")
  }

  # plotting starts here
  vpc_db <-
    as_vpcdb(
      sim = sim,
      sim_km = sim_km,
      obs = obs,
      obs_km = obs_km,
      all_dat = all_dat,
      stratify_pars = stratify_pars,
      stratify = stratify,
      stratify_color = stratify_color,
      stratify_original = stratify_original,
      bins = bins,
      facet = facet,
      labeller = labeller,
      kmmc = kmmc,
      cens_dat = cens_dat,
      rtte = rtte,
      type = "time-to-event",
      as_percentage = as_percentage,
      tmp_bins = tmp_bins,
      xlab = ifelse(is.null(xlab), cols$obs$idv, xlab),
      ylab = ifelse(is.null(ylab), cols$obs$dv, ylab),
      show = show
    )
  if (vpcdb) {
    return(vpc_db)
  } else {
    msg("Plotting...", verbose=verbose)
    pl <- plot_vpc(vpc_db,
                   vpc_theme = vpc_theme,
                   smooth = smooth,
                   log_y = FALSE,
                   title = title)
    return(pl)
  }
}
