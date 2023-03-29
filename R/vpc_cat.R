#' VPC function for categorical
#'
#' Creates a VPC plot from observed and simulation data for categorical variables.
#'
#' @inheritParams format_vpc_input_data
#' @inheritParams read_vpc
#' @inheritParams plot_vpc
#' @inheritParams as_vpcdb
#' @inheritParams define_bins
#' @param bin_mid either "mean" for the mean of all timepoints (default) or "middle" to use the average of the bin boundaries.
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param facet either "wrap", "columns", or "rows"
#' @param labeller ggplot2 labeller function to be passed to underlying ggplot object
#' @param vpcdb boolean whether to return the underlying vpcdb rather than the plot
#' @param verbose show debugging information (TRUE or FALSE)
#' @return a list containing calculated VPC information (when vpcdb=TRUE), or a ggplot2 object (default)
#' @export
#' @seealso \link{sim_data}, \link{vpc}, \link{vpc_tte}, \link{vpc_cens}
#' @examples
#'
#' ## See vpc.ronkeizer.com for more documentation and examples
#' library(vpc)
#'
#' # simple function to simulate categorical data for single individual
#' sim_id <- function(id = 1) {
#'   n <- 10
#'   logit <- function(x) exp(x) / (1+exp(x))
#'   data.frame(id = id, time = seq(1, n, length.out = n),
#'              dv = round(logit((1:n) - n/2 + rnorm(n, 0, 1.5))) )
#' }
#' ## simple function to simulate categorical data for a trial
#' sim_trial <- function(i = 1, n = 20) { # function to simulate categorical data for a trial
#'   data.frame(sim = i, do.call("rbind", lapply(1:n, sim_id)))
#' }
#'
#' ## simulate single trial for 20 individuals
#' obs <- sim_trial(n = 20)
#'
#' ## simulate 200 trials of 20 individuals
#' sim <- do.call("rbind", lapply(1:200, sim_trial, n = 20))
#'
#' ## Plot categorical VPC
#' vpc_cat(sim = sim, obs = obs)
vpc_cat  <- function(sim = NULL,
                     obs = NULL,
                     psn_folder = NULL,
                     bins = "jenks",
                     n_bins = "auto",
                     bin_mid = "mean",
                     obs_cols = NULL,
                     sim_cols = NULL,
                     software = "auto",
                     show = NULL,
                     ci = c(0.05, 0.95),
                     uloq = NULL,
                     lloq = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     title = NULL,
                     smooth = TRUE,
                     vpc_theme = NULL,
                     facet = "wrap",
                     labeller = NULL,
                     vpcdb = FALSE,
                     verbose = FALSE) {
  vpc_data <-
    read_vpc(
      sim=sim, obs=obs, psn_folder=psn_folder,
      software=software,
      sim_cols=sim_cols, obs_cols=obs_cols
    )
  software_type <- vpc_data$software
  cols <- vpc_data$cols

  ## parse data into specific format
  if(!is.null(vpc_data$obs)) {
    vpc_data$obs <-
      format_vpc_input_data(
        dat=vpc_data$obs,
        cols=cols$obs,
        lloq=lloq, uloq=uloq,
        stratify=NULL,
        log_y=FALSE, log_y_min=0,
        what="observed",
        verbose=verbose
      )
  }
  if(!is.null(vpc_data$sim)) {
    vpc_data$sim <-
      format_vpc_input_data(
        dat=vpc_data$sim,
        cols=cols$sim,
        lloq=lloq, uloq=uloq,
        stratify=NULL,
        log_y=FALSE, log_y_min=0,
        what="simulated",
        verbose=verbose
      )
    vpc_data$sim$sim <- add_sim_index_number(vpc_data$sim, id = "id")
  }

  # Binning ####
  bins_data <- define_bins(obs=vpc_data$obs, sim=vpc_data$sim, bins=bins, n_bins=n_bins, verbose=verbose)
  bins <- bins_data$bins
  obs <- bins_data$obs
  sim <- bins_data$sim

  ## parsing
  fact_perc <- function(x, fact) { sum(x == fact) / length(x) } # below lloq, default
  obs$dv <- as.factor(obs$dv)
  lev <- levels(obs$dv)
  if (!is.null(sim)) {
    tmp1 <- sim %>%
      dplyr::group_by(sim, bin)
    for (i in seq(lev)) {
      if (i == 1) {
        aggr_sim <- tmp1 %>%
          dplyr::summarise(fact_perc(dv, lev[i]))
      } else {
        suppressMessages({
          aggr_sim <- dplyr::bind_cols(aggr_sim, tmp1 %>%
                              dplyr::summarise(fact_perc(dv, lev[i])) %>%
                              dplyr::ungroup() %>%
                              dplyr::select(-sim, -bin))
        })
      }
    }
    aggr_sim <- dplyr::bind_cols(aggr_sim, tmp1 %>%
                        dplyr::summarise(mean(idv)) %>%
                        dplyr::ungroup() %>%
                        dplyr::select(-sim, -bin))
    colnames(aggr_sim) <- c("sim", "bin", paste0("fact_", lev), "mn_idv")
    tmp3 <- tidyr::pivot_longer(aggr_sim, names_to = "strat", cols = paste0("fact_", lev)) %>%
      dplyr::arrange(sim, strat, bin) %>%
      dplyr::mutate(strat = stringr::str_replace(strat, "fact_", ""))
    vpc_dat <- tmp3 %>%
      dplyr::group_by(strat, bin) %>%
      dplyr::summarise(q50.low = quantile(value, ci[1]),
                       q50.med = quantile(value, 0.5),
                       q50.up = quantile(value, ci[2]),
                       bin_mid = mean(mn_idv)) %>%
      dplyr::ungroup()
    vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$strat)))[vpc_dat$bin]
    if(bin_mid == "middle") {
      vpc_dat$bin_mid <- apply(cbind(vpc_dat$bin_min, vpc_dat$bin_max), 1, mean)
    }
  } else {
    vpc_dat <- NULL
  }
  if(!is.null(obs)) {
    tmp <- obs %>% dplyr::group_by(bin)
    for (i in seq(lev)) {
      if (i == 1) {
        aggr_obs <- tmp %>%
          dplyr::summarise(fact_perc(dv, lev[i]))
      } else {
        aggr_obs <- cbind(aggr_obs, tmp %>%
                            dplyr::summarise(fact_perc(dv, lev[i])) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-bin) )
      }
    }
    tmp1 <- cbind(aggr_obs,  tmp %>%
                               dplyr::summarise(mean(idv)) %>%
                               dplyr::ungroup() %>%
                               dplyr::select(-bin))
    colnames(tmp1) <- c("bin", paste0("fact_", lev), "bin_mid")
    tmp2 <- tidyr::pivot_longer(tmp1, names_to = "strat", cols = paste0("fact_", lev)) %>%
      dplyr::arrange(strat, bin) %>%
      dplyr::mutate(strat = stringr::str_replace(strat, "fact_", ""))
    tmp2$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(tmp2$strat)) )[tmp2$bin]
    tmp2$bin_max <- rep(bins[2:length(bins)], length(unique(tmp2$strat)) )[tmp2$bin]
    if(bin_mid == "middle") {
      tmp2$bin_mid <- apply(dplyr::bind_cols(tmp2$bin_min, tmp2$bin_max), 1, mean)
    }
    aggr_obs <- tmp2
    colnames(aggr_obs)[4] <- "obs50"
  } else {
    aggr_obs <- NULL
  }

  ## plotting starts here
  vpc_db <-
    as_vpcdb(
      sim = sim,
      vpc_dat = vpc_dat,
      stratify = "strat", # the stratification is the various categories!
      stratify_original = "strat",
      aggr_obs = aggr_obs,
      obs = obs,
      bins = bins,
      facet = facet,
      labeller = labeller,
      type = "categorical",
      xlab = ifelse(is.null(xlab), cols$obs$idv, xlab),
      ylab = ifelse(is.null(ylab), cols$obs$dv, ylab),
      show = show
    )
  if(vpcdb) {
    return(vpc_db)
  } else {
    msg("Plotting...", verbose=verbose)
    pl <- plot_vpc(db = vpc_db,
                   vpc_theme = vpc_theme,
                   smooth = smooth,
                   log_y = FALSE,
                   title = title)
    pl <- pl + ggplot2::ylim(c(0,1))
    return(pl)
  }
}
