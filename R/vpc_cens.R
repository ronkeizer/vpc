#' VPC function for left- or right-censored data (e.g. BLOQ data)
#'
#' Creates a VPC plot from observed and simulation data
#' sim,
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param psn_folder instead of specyfing "sim" and "obs", specify a PsN-generated VPC-folder
#' @param bins either "density", "time", or "data", or a numeric vector specifying the bin separators.
#' @param n_bins number of bins
#' @param bin_mid either "mean" for the mean of all timepoints (default) or "middle" to use the average of the bin boundaries.
#' @param obs_cols observation dataset column names (list elements: "dv", "idv", "id", "pred")
#' @param sim_cols simulation dataset column names (list elements: "dv", "idv", "id", "pred")
#' @param show what to show in VPC (obs_ci, pi, pi_as_area, pi_ci, obs_median, sim_median, sim_median_ci)
#' @param software name of software platform using (eg nonmem, phoenix)
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied.
#' @param stratify_color variable to stratify and color lines for observed data. Only 1 stratification variables can be supplied.
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param uloq Number or NULL indicating upper limit of quantification. Default is NULL.
#' @param lloq Number or NULL indicating lower limit of quantification. Default is NULL.
#' @param plot Boolean indacting whether to plot the ggplot2 object after creation. Default is FALSE.
#' @param xlab ylab as numeric vector of size 2
#' @param ylab ylab as numeric vector of size 2
#' @param title title
#' @param smooth "smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.
#' @param vpc_theme theme to be used in VPC. Expects list of class vpc_theme created with function vpc_theme()
#' @param ggplot_theme specify a custom ggplot2 theme
#' @param facet either "wrap", "columns", or "rows"
#' @param vpcdb boolean whether to return the underlying vpcdb rather than the plot
#' @param verbose show debugging information (TRUE or FALSE)
#' @return a list containing calculated VPC information, and a ggplot2 object
#' @export
#' @seealso \link{vpc}
vpc_cens <- function(sim = NULL,
                     obs = NULL,
                     psn_folder = NULL,
                     bins = "jenks",
                     n_bins = 8,
                     bin_mid = "mean",
                     obs_cols = NULL,
                     sim_cols = NULL,
                     software = "auto",
                     show = NULL,
                     stratify = NULL,
                     stratify_color = NULL,
                     ci = c(0.05, 0.95),
                     uloq = NULL,
                     lloq = NULL,
                     plot = FALSE,
                     xlab = NULL,
                     ylab = NULL,
                     title = NULL,
                     smooth = TRUE,
                     vpc_theme = NULL,
                     ggplot_theme = NULL,
                     facet = "wrap",
                     vpcdb = FALSE,
                     verbose = FALSE) {
  if (is.null(uloq) & is.null(lloq)) {
    stop("You have to specify either a lower limit of quantification (lloq=...) or an upper limit (uloq=...).")
  }
  if (!is.null(uloq) & !is.null(lloq)) {
    stop("You have to specify either a lower limit of quantification (lloq=...) or an upper limit (uloq=...), but you can't specify both.")
  }
  if (is.null(lloq)) {
    type <- "right-censored"
  }
  if (is.null(uloq)) {
    type <- "left-censored"
  }
  if(is.null(obs) & is.null(sim)) {
    stop("At least a simulation or an observation dataset are required to create a plot!")
  }
  if(!is.null(psn_folder)) {
    if(!is.null(obs)) {
      obs <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="original.npctab")[1]))
    }
    if(!is.null(sim)) {
      sim <- read_table_nm(paste0(psn_folder, "/m1/", dir(paste0(psn_folder, "/m1"), pattern="simulation.1.npctab")[1]))
    }
    software = "nonmem"
  }
  if (!is.null(obs)) {
    software_type <- guess_software(software, obs)
  } else {
    software_type <- guess_software(software, sim)
  }

  ## checking whether stratification columns are available
  if(!is.null(stratify)) {
    check_stratification_columns_available(obs, stratify, "observation")
    check_stratification_columns_available(sim, stratify, "simulation")
  }
  if(!is.null(stratify_color)) {
    check_stratification_columns_available(obs, stratify_color, "observation")
    check_stratification_columns_available(sim, stratify_color, "simulation")
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

  ## parse data into specific format
  if(!is.null(obs)) {
    obs <- filter_dv(obs, verbose)
    obs <- format_vpc_input_data(obs, cols$obs, lloq, uloq, stratify, bins, FALSE, 0, "observed", verbose)
  }
  if(!is.null(sim)) {
    sim <- filter_dv(sim, verbose)
    sim <- format_vpc_input_data(sim, cols$sim, lloq, uloq, stratify, bins, FALSE, 0, "simulated", verbose)
    # add sim index number
    sim$sim <- add_sim_index_number(sim)
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
  if(!is.null(obs)) {
    obs <- bin_data(obs, bins, "idv")
  }
  if(!is.null(sim)) {
    sim <- bin_data(sim, bins, "idv")
  }

  ## functions describing censoring %
  loq_perc <- function(x) { sum(x <= lloq) / length(x) } # below lloq, default
  if (is.null(lloq)) {
    loq_perc <- function(x) { sum(x >= uloq) / length(x) }
  }

  ## Parsing data to get the quantiles for the VPC
  if (!is.null(sim)) {
    tmp1 <- sim %>% group_by(strat, sim, bin)
    aggr_sim <- data.frame(cbind(tmp1 %>% summarize(loq_perc(dv)),
                                 tmp1 %>% summarize(mean(idv))))
    colnames(aggr_sim)[grep("loq_perc", colnames(aggr_sim))] <- "ploq"
    colnames(aggr_sim)[length(aggr_sim[1,])] <- c("mn_idv")
    tmp <- aggr_sim %>% group_by(strat, bin)
    vpc_dat <- data.frame(cbind(tmp %>% summarize(quantile(ploq, ci[1])),
                                tmp %>% summarize(quantile(ploq, 0.5)),
                                tmp %>% summarize(quantile(ploq, ci[2])),
                                tmp %>% summarize(mean(mn_idv))
                                ))
    vpc_dat <- vpc_dat[,-grep("(bin.|strat.)", colnames(vpc_dat))]
    colnames(vpc_dat) <- c("strat", "bin", "q50.low","q50.med","q50.up", "bin_mid")
    vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    if(bin_mid == "middle") {
      vpc_dat$bin_mid <- apply(cbind(vpc_dat$bin_min, vpc_dat$bin_max), 1, mean)
    }
  } else {
    vpc_dat <- NULL
  }
  if(!is.null(obs)) {
    tmp <- obs %>% group_by(strat,bin)
    aggr_obs <- data.frame(cbind(tmp %>% summarize(loq_perc(dv)),
                                 tmp %>% summarize(mean(idv))))
    aggr_obs <- aggr_obs[,-grep("(bin.|strat.|sim.)", colnames(aggr_obs))]
    colnames(aggr_obs) <- c("strat", "bin", "obs50")
    colnames(aggr_obs)[length(aggr_obs[1,])] <- c("bin_mid")
    aggr_obs$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
    aggr_obs$bin_max <- rep(bins[2:length(bins)], length(unique(aggr_obs$strat)) )[aggr_obs$bin]
    if(bin_mid == "middle") {
      aggr_obs$bin_mid <- apply(cbind(aggr_obs$bin_min, aggr_obs$bin_max), 1, mean)
    }
  } else {
    aggr_obs <- NULL
  }
  if (!is.null(stratify_original)) {
    if (length(stratify) == 2) {
      vpc_dat$strat1 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)-1]
      vpc_dat$strat2 <- unlist(strsplit(as.character(vpc_dat$strat), ", "))[(1:length(vpc_dat$strat)*2)]
      aggr_obs$strat1 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)-1]
      aggr_obs$strat2 <- unlist(strsplit(as.character(aggr_obs$strat), ", "))[(1:length(aggr_obs$strat)*2)]
    }
  }
  if(is.null(vpc_theme) || (class(vpc_theme) != "vpc_theme")) {
    vpc_theme <- new_vpc_theme()
  }

  ## plotting starts here
  show$median_ci = FALSE
  show$obs_dv = FALSE
  show$obs_ci = FALSE
  show$sim_median = TRUE
  show$sim_median_ci = TRUE
  show$pi_as_area = FALSE
  show$pi_ci = FALSE
  show$pi = FALSE
  vpc_db <- list(sim = sim,
                 vpc_dat = vpc_dat,
                 vpc_theme = vpc_theme,
                 show = show,
                 smooth = smooth,
                 stratify = stratify,
                 stratify_original = stratify_original,
                 stratify_color = stratify_color,
                 aggr_obs = aggr_obs,
                 obs = obs,
                 bins = bins,
                 xlab = xlab,
                 ylab = ylab,
                 log_y = FALSE,
                 facet = facet,
                 title = title,
                 theme = theme,
                 ggplot_theme = ggplot_theme,
                 plot = plot)
  if(vpcdb) return(vpc_db)
  pl <- plot_vpc(vpc_db)
  return(pl)
}
