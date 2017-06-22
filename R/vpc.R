#' VPC function
#'
#' Creates a VPC plot from observed and simulation data
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param psn_folder instead of specyfing "sim" and "obs", specify a PsN-generated VPC-folder
#' @param bins either "density", "time", or "data", "none", or one of the approaches available in classInterval() such as "jenks" (default) or "pretty", or a numeric vector specifying the bin separators.
#' @param n_bins when using the "auto" binning method, what number of bins to aim for
#' @param bin_mid either "mean" for the mean of all timepoints (default) or "middle" to use the average of the bin boundaries.
#' @param obs_cols observation dataset column names (list elements: "dv", "idv", "id", "pred")
#' @param sim_cols simulation dataset column names (list elements: "dv", "idv", "id", "pred")
#' @param show what to show in VPC (obs_dv, obs_ci, pi, pi_as_area, pi_ci, obs_median, sim_median, sim_median_ci)
#' @param software name of software platform using (eg nonmem, phoenix)
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied.
#' @param stratify_color variable to stratify and color lines for observed data. Only 1 stratification variables can be supplied.
#' @param pred_corr perform prediction-correction?
#' @param pred_corr_lower_bnd lower bound for the prediction-correction
#' @param pi simulated prediction interval to plot. Default is c(0.05, 0.95),
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.
#' @param log_y Boolean indicting whether y-axis should be shown as logarithmic. Default is FALSE.
#' @param log_y_min minimal value when using log_y argument. Default is 1e-3.
#' @param xlab ylab as numeric vector of size 2
#' @param ylab ylab as numeric vector of size 2
#' @param title title
#' @param facet_names show facet names (e.g. "SEX=1" when TRUE) or just the value of the facet
#' @param smooth "smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.
#' @param vpc_theme theme to be used in VPC. Expects list of class vpc_theme created with function vpc_theme()
#' @param facet either "wrap", "columns", or "rows"
#' @param vpcdb Boolean whether to return the underlying vpcdb rather than the plot
#' @param verbose show debugging information (TRUE or FALSE)
#' @return a list containing calculated VPC information (when vpcdb=TRUE), or a ggplot2 object (default)
#' @export
#' @seealso \link{sim_data}, \link{vpc_cens}, \link{vpc_tte}
vpc <- function(sim = NULL,
                obs = NULL,
                psn_folder = NULL,
                bins = "jenks",
                n_bins = "auto",
                bin_mid = "mean",
                obs_cols = NULL,
                sim_cols = NULL,
                software = "auto",
                show = NULL,
                stratify = NULL,
                stratify_color = NULL,
                pred_corr = FALSE,
                pred_corr_lower_bnd = 0,
                pi = c(0.05, 0.95),
                ci = c(0.05, 0.95),
                uloq = NULL,
                lloq = NULL,
                log_y = FALSE,
                log_y_min = 1e-3,
                xlab = NULL,
                ylab = NULL,
                title = NULL,
                facet_names = TRUE,
                smooth = TRUE,
                vpc_theme = NULL,
                facet = "wrap",
                vpcdb = FALSE,
                verbose = FALSE) {
  if(!is.null(psn_folder)) {
    if(is.null(obs)) {
      if(verbose) {
        message("Reading oberved data...")
      }
      obs <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="original.npctab")[1]))
    }
    if(is.null(sim)) {
      if(verbose) {
        message("Reading simulated data...")
      }
      sim <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="simulation.1.npctab")[1]))
    }
    software <- "nonmem"
  }
  if(is.null(obs) & is.null(sim)) {
    stop("At least a simulation or an observation dataset are required to create a plot!")
  }
  if(verbose) {
    message("Configuring and initializing...")
  }
  if (!is.null(obs)) {
    software_type <- guess_software(software, obs)
  } else {
    software_type <- guess_software(software, sim)
  }

  ## software specific parsing, if necessary
  if (software_type == "PKPDsim") {
    if (!is.null(obs)) {
      if("obs" %in% obs$comp) {
        obs <- obs %>% dplyr::filter(comp == "obs")
      }
      obs <- data.frame(obs)
    }
    if (!is.null(sim)) {
      if("obs" %in% sim$comp) {
        sim <- sim %>% dplyr::filter(comp == "obs")
      }
      sim <- data.frame(sim)
    }
  }

  ## define what to show in plot
  show <- replace_list_elements(show_default, show)

  ## define column names
  cols <- define_data_columns(sim, obs, sim_cols, obs_cols, software_type)
  if(!is.null(obs)) {
    old_class <- class(obs)
    class(obs) <- c(software_type, old_class)
  }
  if(!is.null(sim)) {
    old_class <- class(sim)
    class(sim) <- c(software_type, old_class)
  }

  ## checking whether stratification columns are available
  if(!is.null(stratify)) {
    if(verbose) {
      message("Stratifying oberved data...")
    }
    if(!is.null(obs)) {
      check_stratification_columns_available(obs, stratify, "observation")
    }
    if(!is.null(sim)) {
      check_stratification_columns_available(sim, stratify, "simulation")
    }
  }
  if(!is.null(stratify_color)) {
    if(verbose) {
      message("Stratifying simulated data...")
    }
    if(!is.null(obs)) {
      check_stratification_columns_available(obs, stratify_color, "observation")
    }
    if(!is.null(sim)) {
      check_stratification_columns_available(sim, stratify_color, "simulation")
    }
  }

  ## parse data into specific format
  if(!is.null(obs)) {
    if(verbose) {
      message("Parsing observed data...")
    }
    obs <- filter_dv(obs, verbose)
    obs <- format_vpc_input_data(obs, cols$obs, lloq, uloq, stratify, bins, log_y, log_y_min, "observed", verbose)
  }
  if(!is.null(sim)) {
    if(verbose) {
      message("Parsing simulated data...")
    }
    sim <- filter_dv(sim, verbose)
    sim <- format_vpc_input_data(sim, cols$sim, lloq, uloq, stratify, bins, log_y, log_y_min, "simulated", verbose)
  }

  stratify_original <- stratify
  if(!is.null(stratify_color)) {
    if (is.null(stratify)) {
      stratify <- stratify_color
    }
    if (length(stratify_color) > 1) {
      stop("Error: please specify only 1 stratification variable for color!")
    }
    if (!stratify_color %in% stratify) {
      stratify_original <- stratify
      stratify <- c(stratify, stratify_color)
    }
  }
  if (class(bins) != "numeric") {
    if(!is.null(obs)) {
      bins <- auto_bin(obs, bins, n_bins)
    } else { # get from sim
      bins <- auto_bin(sim, bins, n_bins)
    }
    if (is.null(bins)) {
      msg("Automatic binning unsuccessful, try increasing the number of bins, or specify vector of bin separators manually.", verbose)
    }
  }
  bins <- unique(bins)
  if(verbose) {
    message(paste0("Binning: ", paste(bins, collapse=' ')))
  }
  if(!is.null(obs)) {
    obs <- bin_data(obs, bins, "idv")
  }
  if(!is.null(sim)) {
    sim <- bin_data(sim, bins, "idv")
  }
  if (pred_corr) {
    if (!is.null(obs) & !cols$obs$pred %in% names(obs)) {
      msg("Warning: Prediction-correction: specified pred-variable not found in observation dataset, trying to get from simulated dataset...", verbose)
      if (!cols$obs$pred %in% names(sim)) {
        stop("Error: Prediction-correction: specified pred-variable not found in simulated dataset, not able to perform pred-correction!")
      } else {
        obs <- obs %>% dplyr::ungroup()
        obs[[cols$obs$pred]] <- unlist(sim[1:length(obs$id), cols$sim$pred])
        msg ("OK", verbose)
      }
    } else {
      if (!cols$sim$pred %in% names(sim)) {
        stop("Warning: Prediction-correction: specified pred-variable not found in simulated dataset, not able to perform pred-correction!")
      }
    }
    if(!is.null(obs)) {
      obs$pred = obs[[cols$obs$pred]]
    }
    if(!is.null(sim)) {
      sim$pred <- sim[[cols$sim$pred]]
    }
  }
  if (!is.null(obs)) {
    if (pred_corr) {
      if(verbose) {
          message("Performing prediction-correction on observed data...")
      }
      obs <- obs %>% dplyr::group_by(strat, bin) %>% dplyr::mutate(pred_bin = median(as.num(pred)))
      obs[obs$pred != 0,]$dv <- pred_corr_lower_bnd + (obs[obs$pred != 0,]$dv - pred_corr_lower_bnd) * (obs[obs$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (obs[obs$pred != 0,]$pred - pred_corr_lower_bnd)
    }
  }
  if (!is.null(sim)) {
    sim$sim <- add_sim_index_number(sim, id = "id")
    if (pred_corr) {
      if(verbose) {
          message("Performing prediction-correction on simulated data...")
      }
      sim <- sim %>% dplyr::group_by(strat, bin) %>% dplyr::mutate(pred_bin = median(pred))
      sim[sim$pred != 0,]$dv <- pred_corr_lower_bnd + (sim[sim$pred != 0,]$dv - pred_corr_lower_bnd) * (sim[sim$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (sim[sim$pred != 0,]$pred - pred_corr_lower_bnd)
    }
  }
  if (!is.null(sim)) {
    if(verbose) {
      message("Calculating statistics for simulated data...")
    }
    tmp1 <- sim %>% dplyr::group_by(strat, sim, bin)
    aggr_sim <- data.frame(cbind(tmp1 %>% dplyr::summarise(quantile(dv, pi[1])),
                                 tmp1 %>% dplyr::summarise(quantile(dv, 0.5 )),
                                 tmp1 %>% dplyr::summarise(quantile(dv, pi[2])),
                                 tmp1 %>% dplyr::summarise(mean(idv))))
    aggr_sim <- aggr_sim[,-grep("(bin.|strat.|sim.)", colnames(aggr_sim))]
    colnames(aggr_sim)[grep("quantile", colnames(aggr_sim))] <- c("q5", "q50", "q95")
    colnames(aggr_sim)[length(aggr_sim[1,])] <- "mn_idv"
    tmp <- aggr_sim %>% dplyr::group_by(strat, bin)
    vpc_dat <- data.frame(cbind(tmp %>% dplyr::summarise(quantile(q5, ci[1])),
                                tmp %>% dplyr::summarise(quantile(q5, 0.5)),
                                tmp %>% dplyr::summarise(quantile(q5, ci[2])),
                                tmp %>% dplyr::summarise(quantile(q50, ci[1])),
                                tmp %>% dplyr::summarise(quantile(q50, 0.5)),
                                tmp %>% dplyr::summarise(quantile(q50, ci[2])),
                                tmp %>% dplyr::summarise(quantile(q95, ci[1])),
                                tmp %>% dplyr::summarise(quantile(q95, 0.5)),
                                tmp %>% dplyr::summarise(quantile(q95, ci[2])),
                                tmp %>% dplyr::summarise(mean(mn_idv))))
    vpc_dat <- vpc_dat[,-grep("(bin.|strat.)", colnames(vpc_dat))]
    colnames(vpc_dat) <- c("strat", "bin",
                           "q5.low","q5.med","q5.up",
                           "q50.low","q50.med","q50.up",
                           "q95.low","q95.med","q95.up",
                           "bin_mid")
    vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    if(bin_mid == "middle") {
      vpc_dat$bin_mid <- apply(cbind(vpc_dat$bin_min, vpc_dat$bin_max), 1, mean)
    }
  } else {
    vpc_dat <- NULL
  }
  if(!is.null(obs)) {
    if(verbose) {
      message("Calculating statistics for observed data...")
    }
    tmp1 <- obs %>% dplyr::group_by(strat,bin)
    aggr_obs <- data.frame(cbind(tmp1 %>% dplyr::summarise(quantile(dv, pi[1])),
                                 tmp1 %>% dplyr::summarise(quantile(dv, 0.5 )),
                                 tmp1 %>% dplyr::summarise(quantile(dv, pi[2])),
                                 tmp1 %>% dplyr::summarise(mean(idv))))
    aggr_obs <- aggr_obs[,-grep("(bin.|strat.|sim.)", colnames(aggr_obs))]
    colnames(aggr_obs) <- c("strat", "bin", "obs5","obs50","obs95", "bin_mid")
    aggr_obs$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
    aggr_obs$bin_max <- rep(bins[2:length(bins)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
    if(bin_mid == "middle") {
      aggr_obs$bin_mid <- apply(cbind(aggr_obs$bin_min, aggr_obs$bin_max), 1, mean)
    }
  } else {
    aggr_obs <- NULL
  }
  if(is.null(xlab)) {
    xlab <- cols$obs$idv
  }
  if(is.null(ylab)) {
    ylab <- cols$obs$dv
  }
  if (!is.null(stratify_original)) {
    if (length(stratify) == 2) {
      vpc_dat$strat1 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)-1]
      vpc_dat$strat2 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)]
      aggr_obs$strat1 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)-1]
      aggr_obs$strat2 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)]
    }
  }
  # data combined and handed off to separate plotting function
  if(verbose) {
    message("Creating plot...")
  }
  vpc_db <- list(sim = sim,
                 vpc_dat = vpc_dat,
                 smooth = smooth,
                 stratify = stratify,
                 stratify_original = stratify_original,
                 stratify_color = stratify_color,
                 aggr_obs = aggr_obs,
                 obs = obs,
                 bins = bins,
                 facet = facet,
                 type = "continuous")
  if(facet_names == FALSE) {
    datasets <- c("vpc_dat", "obs", "sim", "aggr_obs")
    for(i in seq(datasets)) {
      for(j in seq(vpc_db$stratify)) {
        vpc_db[[datasets[i]]]$strat <- as.factor(gsub(paste0(vpc_db$stratify[j],"="), "", vpc_db[[datasets[i]]]$strat))
      }
    }
  }
  if(vpcdb) {
    return(vpc_db)
  } else {
    pl <- plot_vpc(vpc_db, 
                   show = show, 
                   vpc_theme = vpc_theme,
                   smooth = smooth,
                   log_y = log_y,
                   title = title,
                   xlab = xlab,
                   ylab = ylab)
    return(pl)
  }
}
