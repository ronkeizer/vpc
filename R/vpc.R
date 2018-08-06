#' VPC function
#'
#' Creates a VPC plot from observed and simulation data
#' @param sim this is usually a data.frame with observed data, containing the independent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}.  However it can also be an object like a nlmixr or xpose object
#' @param obs a data.frame with observed data, containing the independent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param psn_folder instead of specifying "sim" and "obs", specify a PsN-generated VPC-folder
#' @param bins either "density", "time", or "data", "none", or one of the approaches available in classInterval() such as "jenks" (default) or "pretty", or a numeric vector specifying the bin separators.
#' @param n_bins when using the "auto" binning method, what number of bins to aim for
#' @param bin_mid either "mean" for the mean of all timepoints (default) or "middle" to use the average of the bin boundaries.
#' @param regression create regression-based VPC? (does not use bins)
#' @param obs_cols observation dataset column names (list elements: "dv", "idv", "id", "pred")
#' @param sim_cols simulation dataset column names (list elements: "dv", "idv", "id", "pred", "sim")
#' @param show what to show in VPC (obs_dv, obs_ci, pi, pi_as_area, pi_ci, obs_median, sim_median, sim_median_ci)
#' @param software name of software platform using (e.g. nonmem, phoenix)
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied.
#' @param pred_corr perform prediction-correction?
#' @param pred_corr_lower_bnd lower bound for the prediction-correction
#' @param pi simulated prediction interval to plot. Default is c(0.05, 0.95),
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.
#' @param log_y Boolean indicting whether y-axis should be shown as logarithmic. Default is FALSE.
#' @param log_y_min minimal value when using log_y argument. Default is 1e-3.
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param title title
#' @param smooth "smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.
#' @param vpc_theme theme to be used in VPC. Expects list of class vpc_theme created with function vpc_theme()
#' @param facet either "wrap", "columns", or "rows"
#' @param labeller ggplot2 labeller function to be passed to underlying ggplot object
#' @param vpcdb Boolean whether to return the underlying vpcdb rather than the plot
#' @param verbose show debugging information (TRUE or FALSE)
#' @param ... Other arguments sent to other methods (like xpose or nlmixr);  Note these arguments are not used in the default vpc and are ignored by the default method.
#' @return a list containing calculated VPC information (when vpcdb=TRUE), or a ggplot2 object (default)
#' @export
#' @seealso \link{sim_data}, \link{vpc_cens}, \link{vpc_tte}, \link{vpc_cat}
#' @examples
#'
#' ## See vpc.ronkeizer.com for more documentation and examples
#' library(vpc)
#'
#' # Basic commands:
#' vpc(sim = simple_data$sim, obs = simple_data$obs)
#' vpc(sim = simple_data$sim, obs = simple_data$obs, lloq = 20)
#'
#'@export
vpc <- function(sim, ...){
    UseMethod("vpc")
}
#' @rdname vpc
#'@export
vpc.default <- function(sim, ...){
    call <- as.list(match.call(expand.dots=TRUE))[-1];
    do.call(getFromNamespace("vpc_vpc","vpc"), call, envir = parent.frame(1))
}
#' @rdname vpc
#'@export
vpc_vpc <- function(sim = NULL,
                    obs = NULL,
                    psn_folder = NULL,
                    regression = FALSE,
                    bins = "jenks",
                    n_bins = "auto",
                    bin_mid = "mean",
                    obs_cols = NULL,
                    sim_cols = NULL,
                    software = "auto",
                    show = NULL,
                    stratify = NULL,
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
                    smooth = TRUE,
                    vpc_theme = NULL,
                    facet = "wrap",
                    labeller = NULL,
                    vpcdb = FALSE,
                    verbose = FALSE, ...) {
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
  if(!is.null(facet)) {
    if(! facet %in% c("wrap", "grid", "columns", "rows")) {
      stop("`facet` argument needs to be one of `wrap`, `columns`, or `rows`.")
    }
    if(facet == "grid") facet <- "rows"
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

  ## Currently we can't handle both LLOQ and ULOQ
  if(!is.null(uloq) && !is.null(lloq)) {
    stop("Sorry, currently the vpc function cannot handle both upper and lower limit of quantification. Please specify either `lloq` or `uloq`.")
  }

  ## parse data into specific format
  if(!is.null(obs)) {
    if(verbose) {
      message("Parsing observed data...")
    }
    obs <- filter_dv(obs, verbose)
    obs <- format_vpc_input_data(obs, cols$obs, lloq, uloq, stratify, bins, log_y, log_y_min, "observed", verbose, pred_corr)
  }
  if(!is.null(sim)) {
    if(verbose) {
      message("Parsing simulated data...")
    }
    sim <- filter_dv(sim, verbose)
    if((!is.null(lloq) || !is.null(uloq)) && pred_corr) {
      message("Prediction-correction cannot be used together with censored data (<LLOQ or >ULOQ). VPC plot will be shown for non-censored data only!")
      sim <- format_vpc_input_data(sim, cols$sim, lloq, uloq, stratify, bins, log_y, log_y_min, "simulated", verbose, pred_corr)
    } else {
      sim <- format_vpc_input_data(sim, cols$sim, NULL, NULL, stratify, bins, log_y, log_y_min, "simulated", verbose, pred_corr)
    }
  }
  if(pred_corr) {
    uloq <- NULL
    lloq <- NULL
  }
  
  if(regression) {
    ############### Regression based VPCs ####################################
    optim_func <- function(data, log_lambda, quant = 0.5) {
      a <- AIC(
        rqss(
          data$dv ~ qss(data$idv, lambda=exp(log_lambda)), tau=quant, na.action=na.exclude), k = -1
        )
    }
    calc_quantiles_aqr <- function(x, lambda = c(0.5, 0.5, 0.5), qs = c(0.05, 0.5, 0.95)) {
      tmp <- x %>% 
        dplyr::filter()
      sim_tmp <- list()
      for(i in seq(qs)) {
        sim_tmp[[paste0("q_", qs[i])]] <- rqss(x$dv ~ qss(x$idv, lambda = exp(lambda[i])), tau = qs[i], na.action = na.exclude)
      }
      x[[paste0("q5")]] <- fitted(sim_tmp[[paste0("q_", qs[1])]])
      x[[paste0("q50")]] <- fitted(sim_tmp[[paste0("q_", qs[2])]])
      x[[paste0("q95")]] <- fitted(sim_tmp[[paste0("q_", qs[3])]])
      x %>%
        dplyr::filter(!duplicated(idv)) %>%
        dplyr::arrange(idv)
    }
    
    lambda <- c()
    qs <- c(pi[1], 0.5, pi[2])
    for(i in seq(qs)) {
      if(verbose) message("Optimizing lambda for quantile (obs): ", qs[i])
      lambda[i] <- optimize(optim_func, data = obs, quant = qs[i], interval=c(-1, 7))$min
    }
    if(verbose) message("Calculating VPC stats for quantiles (obs)")
    aggr_obs <- calc_quantiles_aqr(obs, lambda = lambda, qs = qs) %>%
      dplyr::mutate(mn_idv = idv) %>%
      dplyr::mutate(bin = match(idv, unique(idv))) %>%
      dplyr::select(strat = strat, bin = bin, obs5 = q5, obs50 = q50, obs95 = q95, bin_mid = mn_idv)

    if(verbose) message("Calculating simulation quantiles...")
    sim$sim <- add_sim_index_number(sim, id = "id", sim_label=sim_cols$sim)
    aggr_sim <- purrr::map(split(sim, sim$sim), calc_quantiles_aqr) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(mn_idv = idv) %>%
      dplyr::mutate(bin = match(idv, unique(idv)))

    # obs %>%
    #   dplyr::filter(!duplicated(idv)) %>%
    #   dplyr::arrange(idv) %>%
    #   ggplot2::ggplot(ggplot2::aes(x = idv, y = q50)) + 
    #     ggplot2::geom_line() + 
    #     ggplot2::geom_line(ggplot2::aes(y = q5), linetype = 'dotted') + 
    #     ggplot2::geom_line(ggplot2::aes(y = q95), linetype = 'dotted') 
  } else {
    ############### Non-Regression based VPCs ####################################
    
    ## Handle binning
    labeled_bins <- bins[1] == "percentiles"
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
    if(verbose) message(paste0("Binning: ", paste(bins, collapse=' ')))
    if(!is.null(obs)) {
      obs <- bin_data(obs, bins, "idv", labeled = labeled_bins)
    }
    if(!is.null(sim)) {
      sim <- bin_data(sim, bins, "idv", labeled = labeled_bins)
    }
    if(pred_corr) {
      if(!is.null(obs) & !cols$obs$pred %in% names(obs)) {
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
        obs$pred <- obs[[cols$obs$pred]]
      }
      if(!is.null(sim)) {
        sim$pred <- sim[[cols$sim$pred]]
      }
    }
    if(!is.null(obs)) {
      if(pred_corr) {
        if(verbose) message("Performing prediction-correction on observed data...")
        obs <- obs %>% dplyr::group_by(strat, bin) %>% dplyr::mutate(pred_bin = median(as.num(pred)))
        obs[obs$pred != 0,]$dv <- pred_corr_lower_bnd + (obs[obs$pred != 0,]$dv - pred_corr_lower_bnd) * (obs[obs$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (obs[obs$pred != 0,]$pred - pred_corr_lower_bnd)
      }
    }
    if(!is.null(sim)) {
      sim$sim <- add_sim_index_number(sim, id = "id", sim_label=sim_cols$sim)
      if(pred_corr) {
        if(verbose) message("Performing prediction-correction on simulated data...")
        sim <- sim %>% dplyr::group_by(strat, bin) %>% dplyr::mutate(pred_bin = median(pred))
        sim[sim$pred != 0,]$dv <- pred_corr_lower_bnd + (sim[sim$pred != 0,]$dv - pred_corr_lower_bnd) * (sim[sim$pred != 0,]$pred_bin - pred_corr_lower_bnd) / (sim[sim$pred != 0,]$pred - pred_corr_lower_bnd)
      }
    }
  }

  if(!is.null(sim)) {
    if(!regression) {
      if(verbose) message("Calculating statistics for simulated data...")
      tmp1 <- sim %>% dplyr::group_by(strat, sim, bin)
      aggr_sim <- data.frame(cbind(tmp1 %>% dplyr::summarise(quantile(dv, pi[1])),
                                   tmp1 %>% dplyr::summarise(quantile(dv, 0.5 )),
                                   tmp1 %>% dplyr::summarise(quantile(dv, pi[2])),
                                   tmp1 %>% dplyr::summarise(mean(idv))))
      aggr_sim <- aggr_sim[,-grep("(bin.|strat.|sim.)", colnames(aggr_sim))]
      colnames(aggr_sim)[grep("quantile", colnames(aggr_sim))] <- c("q5", "q50", "q95")
      colnames(aggr_sim)[length(aggr_sim[1,])] <- "mn_idv"
    }
    tmp <- aggr_sim %>% 
      dplyr::group_by(strat, bin)
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
    if(!regression) {
      vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
      vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    } else {
      vpc_dat <- vpc_dat %>%
        dplyr::mutate(bin_min = bin_mid, bin_max = bin_mid)
    }
    if(bin_mid == "middle") {
      vpc_dat$bin_mid <- apply(cbind(vpc_dat$bin_min, vpc_dat$bin_max), 1, mean)
    }
  } else {
    vpc_dat <- NULL
  }
  if(!is.null(obs)) {
    if(!regression) {
      if(verbose) {
        message("Calculating statistics for observed data...")
      }
      tmp1 <- obs %>% 
        dplyr::group_by(strat, bin)
      if(!is.null(lloq) || !is.null(uloq)) {
        if(!is.null(uloq)) { limit <- uloq; cens = "right" }
        if(!is.null(lloq)) { limit <- lloq; cens = "left" }
        aggr_obs <- data.frame(cbind(tmp1 %>% dplyr::summarise(quantile_cens(dv, pi[1], limit = limit, cens = cens)),
                                     tmp1 %>% dplyr::summarise(quantile_cens(dv, 0.5, limit = limit, cens = cens)),
                                     tmp1 %>% dplyr::summarise(quantile_cens(dv, pi[2], limit = limit, cens = cens)),
                                     tmp1 %>% dplyr::summarise(mean(idv))))
      } else {
        aggr_obs <- data.frame(cbind(tmp1 %>% dplyr::summarise(quantile(dv, pi[1])),
                                     tmp1 %>% dplyr::summarise(quantile(dv, 0.5)),
                                     tmp1 %>% dplyr::summarise(quantile(dv, pi[2])),
                                     tmp1 %>% dplyr::summarise(mean(idv))))
      }
      aggr_obs <- aggr_obs[,-grep("(bin.|strat.|sim.)", colnames(aggr_obs))]
      colnames(aggr_obs) <- c("strat", "bin", "obs5","obs50","obs95", "bin_mid")
    }
    if(!regression) {
      aggr_obs$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
      aggr_obs$bin_max <- rep(bins[2:length(bins)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
      if(bin_mid == "middle") {
        aggr_obs$bin_mid <- apply(cbind(aggr_obs$bin_min, aggr_obs$bin_max), 1, mean)
      }
    } else {
      aggr_obs <- aggr_obs %>%
        dplyr::mutate(bin_min = bin_mid, bin_max = bin_mid)
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
  if(!is.null(stratify)) {
    if(length(stratify) == 2) {
      vpc_dat$strat1 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)-1]
      vpc_dat$strat2 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)]
      aggr_obs$strat1 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)-1]
      aggr_obs$strat2 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)]
    }
  }
  # data combined and handed off to separate plotting function
  if(verbose & !vpcdb) {
    message("Creating plot...")
  }
  vpc_db <- list(sim = sim,
                 vpc_dat = vpc_dat,
                 smooth = smooth,
                 stratify = stratify,
                 aggr_obs = aggr_obs,
                 obs = obs,
                 bins = bins,
                 facet = facet,
                 labeller = labeller,
                 lloq = lloq,
                 uloq = uloq,
                 type = "continuous",
                 xlab = xlab,
                 ylab = ylab,
                 regression = regression)
  if(vpcdb) {
    return(vpc_db)
  } else {
    pl <- plot_vpc(vpc_db,
                   show = show,
                   vpc_theme = vpc_theme,
                   smooth = smooth,
                   log_y = log_y,
                   title = title)
    return(pl)
  }
}
