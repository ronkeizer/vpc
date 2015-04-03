#' VPC function for survival-type data 
#' 
#' Creates a VPC plot from observed and simulation survival data
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param rtte repeated time-to-event data? Deafult is FALSE (treat as single-event TTE)
#' @param rtte_calc_diff recalculate time (T/F)? When simulating in NONMEM, you will probably need to set this to TRUE to recalculate the TIME to relative times between events (unless you output the time difference between events and specify that as independent variable to the vpc_tte() funciton.
#' @param events numeric vector describing which events to show a VPC for when repeated TTE data, e.g. c(1:4). Default is NULL, which shows all events. 
#' @param pi_med plot the median of the simulated data? Default is FALSE.
#' @param obs_dv variable in data.frame for observed dependent value. "dv" by default
#' @param sim_dv variable in data.frame for simulated dependent value. "sdv" by default
#' @param obs_idv variable in data.frame for observed independent value. "time" by default
#' @param sim_idv variable in data.frame for simulated independent value. "time" by default
#' @param obs_id variable in data.frame for observed individual. "id" by default
#' @param sim_id variable in data.frame for simulated individual. "id" by default
#' @param nonmem should variable names standard to NONMEM be used (i.e. ID, TIME, DV, PRED). Default is "auto" for autodetect.
#' @param dense_grid Was a dense grid used to simulate and should the vpc_tte function remove the non-event data? TRUE or FALSE. Either way should not affect plot, when points removed the plot will just be generated faster.
#' @param reverse_prob reverse the probability scale (i.e. plot 1-probability)
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied. If stratify_color is also specified, only 1 additional stratification can be specified.
#' @param stratify_color variable to stratify and color lines for observed data. Only 1 stratification variables can be supplied.
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param plot Boolean indacting whether to plot the ggplot2 object after creation. Default is FALSE.
#' @param xlab ylab as numeric vector of size 2
#' @param ylab ylab as numeric vector of size 2
#' @param title title
#' @param smooth "smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.
#' @param vpc_theme theme to be used in VPC. Expects list of class vpc_theme created with function vpc_theme()
#' @param ggplot_theme specify a custom ggplot2 theme
#' @param facet either "wrap", "columns", or "rows" 
#' @param verbose TRUE or FALSE (default)
#' @return a list containing calculated VPC information, and a ggplot2 object
#' @export
#' @seealso \link{vpc}
#' @examples
#' ## Example for repeated) time-to-event data
#' ## with NONMEM-like data (e.g. simulated using a dense grid)
#' data(rtte_obs_nm) 
#' data(rtte_sim_nm) 
#' 
#' # treat RTTE as TTE, no stratification
#' vpc_tte(sim = rtte_sim_nm, 
#'         obs = rtte_obs_nm, 
#'         rtte = FALSE, 
#'         sim_dv = "dv", obs_idv = "t", sim_idv = "t")
#' 
#' # stratified for covariate and study arm
#' vpc_tte(sim = rtte_sim_nm, 
#'         obs = rtte_obs_nm, 
#'         stratify = c("sex","drug"), 
#'         rtte = FALSE,
#'         sim_dv = "dv", obs_idv = "t", sim_idv = "t")
#' 
#' # stratified per event number (we'll only look at first 3 events) and stratify per arm
#' vpc_tte(sim = rtte_sim_nm, 
#'         obs = rtte_obs_nm,
#'         rtte = TRUE, events = c(1:3),
#'         stratify = c("drug"), 
#'         sim_dv = "dv", obs_idv = "t", sim_idv = "t")
vpc_tte <- function(sim = NULL, 
                    obs = NULL, 
                    psn_folder = NULL,
                    rtte = FALSE,
                    rtte_calc_diff = TRUE,
                    events = NULL,
                    bins = FALSE,
                    n_bins = 10,
                    software = "auto",
                    obs_cols = NULL,
                    sim_cols = NULL,
                    kmmc = NULL,
                    pi_med = FALSE, 
                    reverse_prob = FALSE,
                    stratify = NULL,
                    stratify_color = NULL,
                    ci = c(0.05, 0.95),
                    plot = FALSE,
                    xlab = NULL, 
                    ylab = NULL,
                    as_percentage = TRUE,
                    title = NULL,
                    smooth = FALSE,
                    vpc_theme = NULL,
                    ggplot_theme = NULL,
                    facet = "wrap",
                    verbose = FALSE) {
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

  ## remove EVID != 0 / MDV != 0
  if(!is.null(obs)) {
    obs <- filter_dv(obs, verbose)
  }
  if(!is.null(sim)) {  
    sim <- filter_dv(sim, verbose)
  }
  
  ## stratification
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
  if(!is.null(stratify)) {
    if(rtte) {
      if (length(stratify) > 1) {
        stop ("Sorry, with repeated time-to-event data, stratification on more than 1 variables is currently not supported!")
        invisible()
      }
    } else {
      if (length(stratify) > 2) {
        stop ("Sorry, stratification on more than 2 variables is currently not supported!")
        invisible()
      }
    }    
  }

  ## format obs data
  if(!is.null(obs)) {
    obs$id <- obs[[cols$obs$id]]
    if (length(obs[[cols$obs$id]]) == 0) {
      msg("Warning: No ID column found, assuming 1 row per ID.", verbose)
      obs$id <- 1:length(obs[,1])
    }
    obs$time <- obs[[cols$obs$idv]]
    obs$dv <- obs[[cols$obs$dv]]
    if(max(obs$dv) > 1) { # guessing DV definition if not just 0/1
      if(max(obs$dv) == 2) { # common approach in NONMEM, 2 = censored
        obs[obs$dv != 1,]$dv <- 0
        msg("Warning: vpc_tte() expected the observed dependent variable to contain only 0 (censored, or no event observed) or 1 (event observed). Setting all observations != 1 to 0.", verbose)
      } else {
        obs[obs$dv != 1,]$dv <- 1 # some people use DV to indicate the event time. 
        msg("Warning: vpc_tte() expected the dependent variable to contain only 0 (censored, or no event observed) or 1 (event observed). Setting all observations != 1 to 1.", verbose)
      }
    }
    if("cens" %in% tolower(colnames(obs))) { # some people use a 'cens' column to indicate censoring
      colnames(obs)[match("cens", tolower(colnames(obs)))] <- "cens"
      msg("Detected extra column with censoring information in observation data, assuming 1=censored event, 0=observed event.", verbose)
      obs[obs$cens == 1,]$dv <- 0
    }
    if (rtte) {
      if(rtte_calc_diff) {
        obs <- relative_times(obs)
      }
      obs <- obs %>% group_by(id) %>% mutate(rtte = cumsum(dv != 0)) 
      stratify <- c(stratify, "rtte")
    } else {
      obs$rtte <- 1
    }
    
    # add stratification column and comput KM curve for observations
    obs <- add_stratification(obs, stratify)
    if(!is.null(kmmc) && kmmc %in% names(obs)) {
      obs_km <- compute_kmmc(obs, strat = "strat", reverse_prob = reverse_prob, kmmc=kmmc)
    } else {
      obs_km <- compute_kaplan(obs, strat = "strat", reverse_prob = reverse_prob)
    }
  } else { # get bins from sim
    obs_km <- NULL
  }
  if(!is.null(kmmc) & (class(bins) == "logical" && bins == FALSE)) {
    msg("Tip: with KMMC-type plots, binning of simulated data is recommended. See documentation for the 'bins' argument for more information.", msg)
  }
    
  if(!is.null(sim)) {
    # format sim data and compute KM curve CI for simulations
    if (all(c(cols$sim$idv, cols$sim$id, cols$sim$dv) %in% names(sim))) {
      sim$id <- sim[[cols$sim$id]]
      sim$dv <- sim[[cols$sim$dv]]    
      sim$time <- sim[[cols$sim$idv]]
    } else {
      stop("Not all required variables were found, please check column definitions for id, dv and time.")  
    }
    if(max(sim$dv) > 2) { # guessing DV definition if not just 0/1
      if(max(sim$dv) == 2) { # common approach in NONMEM, 2 = censored
        sim[sim$dv != 1,]$dv <- 1
        msg("Warning: Expected simulated dependent variable to contain only 0 (censored, or no event simerved) or 1 (event simerved). Setting all simulated observations != 1 to 0.", msg)
      } else {
        sim[sim$dv != 1,]$dv <- 1 # some people use DV to indicate the event time. 
        msg("Warning: Expected simulated dependent variable to contain only 0 (censored, or no event simerved) or 1 (event simerved). Setting all simulated observations != 1 to 1.", msg)
      }
    }
    if(max(sim$dv) == 1) {
      if (sum(sim$dv > 0 & sim$dv < 1) > 0) {
        sim[sim$dv > 0  & sim$dv < 1,]$dv <- 0 
      }
    }
    if("cens" %in% tolower(names(sim$cens))) { # some people use a 'cens' column to indicate censoring
      cat("Detected extra column with censoring information in simulation data.")
      colnames(sim)[match("cens", tolower(colnames(sim)))] <- "cens"
      sim[sim$cens == 1,]$dv <- 0
    }
    # add sim index number
    sim$sim <- add_sim_index_number(sim, id = cols$sim$id)
      
    # set last_observation and repeat_obs per sim&id
    sim <- sim %>% group_by(sim, id) %>% mutate(last_obs = 1*(1:length(time) == length(time)), repeat_obs = 1*(cumsum(dv) > 1))  

    # filter out stuff and recalculate rtte times
    sim <- sim[sim$dv == 1 | (sim$last_obs == 1 & sim$dv == 0),]
    if(rtte) {
      sim <- sim %>% dplyr::group_by(sim, id) %>% dplyr::mutate(rtte = cumsum(dv != 0)) %>% arrange(sim, id)       
      if(rtte_calc_diff) {
        sim <- relative_times(sim, simulation=TRUE)
      }
    } else {
      sim$sim_id <- paste0(sim$sim, "_", sim$id) # remove duplicate observations rows per id to filter out repeated obs
      sim <- sim[!duplicated(sim$sim_id),]  
    }

    n_sim <- length(unique(sim$sim))        
    all <- c()
    tmp_bins <- unique(c(0, sort(unique(sim$time)), max(sim$time))) 
    if(!(class(bins) == "logical" && bins == FALSE)) {
      if(class(bins) == "logical" && bins == TRUE) { 
        bins <- "time" 
      } 
      if(class(bins) == "character") {
        if (bins == "obs") {
          tmp_bins <- unique(c(0, sort(unique(obs$time)), max(obs$time))) 
        } else {
          if (!(bins %in% c("time","data"))) {
            msg(paste0("Note: bining method ", bins," might be slow. Consider using method 'time', or specify 'bins' as numeric vector"), verbose)
          }
          tmp_bins <- unique(c(0, auto_bin(sim %>% mutate(idv=time), type=bins, n_bins = n_bins-1), max(sim$time)))
        }
      }
      if(class(bins) == "numeric") {
        tmp_bins <- unique(c(0, bins, max(obs$time))) 
      }
    }
    for (i in 1:n_sim) {
      tmp <- sim %>% dplyr::filter(sim == i)
      tmp2 <- add_stratification(tmp %>% dplyr::arrange(id, time), stratify)
      if(!is.null(kmmc) && kmmc %in% names(obs)) {
        tmp3 <- compute_kmmc(tmp2, strat = "strat", reverse_prob = reverse_prob, kmmc = kmmc)
      } else {
        tmp3 <- compute_kaplan(tmp2, strat = "strat", reverse_prob = reverse_prob)  
      }
      tmp3[,c("bin", "bin_min", "bin_max", "bin_mid")] <- 0 
      tmp3$bin <- cut(tmp3$time, breaks = tmp_bins, labels = FALSE, right = TRUE)
      tmp3$bin_min <- tmp_bins[tmp3$bin] 
      tmp3$bin_max <- tmp_bins[tmp3$bin+1] 
      tmp3$bin_mid <- (tmp3$bin_min + tmp3$bin_max) / 2      
      all <- rbind(all, cbind(i, tmp3))
    }
    sim_km <- all %>% 
      dplyr::group_by (strat, bin) %>% 
      dplyr::summarize (bin_mid = head(bin_mid,1), bin_min = head(bin_min,1), bin_max = head(bin_max,1), 
                 qmin = quantile(surv, 0.05), qmax = quantile(surv, 0.95), qmed = median(surv),
                 step = 0)
  } else {
    sim_km <- NULL
  }

  if (rtte) {
    if(!is.null(sim)) {
      sim_km$rtte <- as.num(gsub(".*rtte=(\\d.*).*", "\\1", sim_km$strat, perl = TRUE))
      if (!is.null(events)) {
        sim_km <- sim_km %>% filter(rtte %in% events)
        # redefine strat factors, since otherwise empty panels will be shown
        sim_km$strat <- factor(sim_km$strat, levels = unique(sim_km$strat))
      }      
    }
    if(!is.null(obs)) {
      obs_km$rtte <- as.num(gsub(".*rtte=(\\d.*).*", "\\1", obs_km$strat, perl = TRUE))
      if (!is.null(events)) {
        obs_km <- obs_km %>% filter(rtte %in% events)
        obs_km$strat <- factor(obs_km$strat, levels = unique(obs_km$strat))
      } 
    }
  }
  
  if (smooth) {
    geom_line_custom <- geom_line
  } else {
    geom_line_custom <- geom_step
  }
  if (!is.null(stratify_original)) {
    if (length(stratify) == 2) {
      sim_km$strat1 <- unlist(strsplit(as.character(sim_km$strat), ", "))[(1:length(sim_km$strat)*2)-1]
      sim_km$strat2 <- unlist(strsplit(as.character(sim_km$strat), ", "))[(1:length(sim_km$strat)*2)]
      obs_km$strat1 <- unlist(strsplit(as.character(obs_km$strat), ", "))[(1:length(obs_km$strat)*2)-1]
      obs_km$strat2 <- unlist(strsplit(as.character(obs_km$strat), ", "))[(1:length(obs_km$strat)*2)]        
    }
  }  
  if (!is.null(sim)) {  
    if (!is.null(stratify_color)) {
      if (length(stratify) == 2) {
        sim_km$strat_color <- sim_km$strat2
      } else {
        sim_km$strat_color <- sim_km$strat  
      }
    }
    if(is.null(vpc_theme) || (class(vpc_theme) != "vpc_theme")) {
      vpc_theme <- create_vpc_theme()
    }
    pl <- ggplot(sim_km, aes(x=bin_mid, y=qmed, group=strat))       
    if (smooth) {
      if (!is.null(stratify_color)) {
        pl <- pl + 
          geom_ribbon(aes(min = qmin, max=qmax, y=qmed, fill=strat_color), alpha=vpc_theme$sim_median_alpha) +
          scale_fill_discrete(name="")
      } else {
        pl <- pl + geom_ribbon(aes(min = qmin, max=qmax, y=qmed), fill = vpc_theme$sim_median_fill, alpha=vpc_theme$sim_median_alpha)        
      }
    } else {
      if (!is.null(stratify_color)) {
        pl <- pl + 
          geom_rect(aes(xmin=bin_min, xmax=bin_max, ymin=qmin, ymax=qmax, fill=strat_color), alpha=vpc_theme$sim_median_alpha) +
          scale_fill_discrete(name="")
      } else {
        pl <- pl + geom_rect(aes(xmin=bin_min, xmax=bin_max, ymin=qmin, ymax=qmax), alpha=vpc_theme$sim_median_alpha, fill = vpc_theme$sim_median_fill)           
      }
    }
    if (pi_med) {
      pl <- pl + geom_line_custom(linetype="dashed")
    }
  } else {
    pl <- ggplot(obs_km)       
  }
  if (!is.null(obs)) {  
    show_obs <- TRUE
    if(!is.null(stratify_color)) {
      if (length(stratify) == 2) {
        obs_km$strat_color <- obs_km$strat2
      } else {
        obs_km$strat_color <- obs_km$strat  
      }
    }    
    if (show_obs) {
      chk_tbl <- obs_km %>% group_by(strat) %>% dplyr::summarize(t = length(time))
      if (sum(chk_tbl$t <= 1)>0) { # it is not safe to use geom_step, so use 
        geom_step <- geom_line
      }
      msg("Warning: some strata in the observed data had zero or one observations, using line instead of step plot. Consider using less strata (e.g. using the 'events' argument).", verbose)        
      if (!is.null(stratify_color)) {
        pl <- pl + 
          geom_step(data = obs_km, aes(x=time, y=surv, colour=strat_color)) +         
          scale_colour_discrete(name="")
      } else {
        pl <- pl + geom_step(data = obs_km, aes(x=time, y=surv, group=strat))         
      }
    }
  }
  if (!is.null(stratify)) {
    if (length(stratify_original) == 1 | rtte) {
      if (!is.null(stratify_color)) {
          if (facet == "wrap") {
            pl <- pl + facet_wrap(~ strat1)      
          } else {
            if(length(grep("row", facet))>0) {
              pl <- pl + facet_grid(strat1 ~ .)                
            } else {
              pl <- pl + facet_grid(. ~ strat1)                
            }
          } 
      } else {
        if (facet == "wrap") {
          pl <- pl + facet_wrap(~ strat)      
        } else {
          if(length(grep("row", facet))>0) {
            pl <- pl + facet_grid(strat ~ .)                
          } else {
            pl <- pl + facet_grid(. ~ strat)                
          }
        }         
      } 
    } else {
      if ("strat1" %in% c(colnames(sim_km), colnames(obs_km))) {
        if(length(grep("row", facet))>0) {
          pl <- pl + facet_grid(strat1 ~ strat2)                
        } else {
          pl <- pl + facet_grid(strat2 ~ strat1)                
        }        
      } else {
        if ("strat" %in% c(colnames(sim_km), colnames(obs_km))) {
          # color stratification only
        } else {          
          stop ("Stratification unsuccesful.")          
        }
      }
    }
  }
  if(!(class(bins) == "logical" && bins == FALSE)) {
    bdat <- data.frame(cbind(x=tmp_bins, y=NA))
    pl <- pl + geom_rug(data=bdat, sides = "t", aes(x = x, y=y, group=NA), colour=vpc_theme$bin_separators_color)
  }
  if (!is.null(title)) {
    pl <- pl + ggtitle(title)  
  }
  if (!is.null(ggplot_theme)) {  
    pl <- pl + ggplot_theme()    
  } else {
    if (!is.null(theme)) {
      pl <- pl + theme_plain()
    } 
  }
  if(!is.null(xlab)) {
    pl <- pl + xlab(xlab)
  } else {
    pl <- pl + xlab("Time")
  }
  if(!is.null(ylab)) {
    pl <- pl + ylab(ylab)
  } else {
    if(is.null(kmmc)) {
      if(as_percentage && is.null(kmmc)) {
        percent <- seq(from=0, to=100, by=25)
        pl <- pl + 
          scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = percent) +
          ylab("Survival (%)")        
      } else {
        pl <- pl + ylab("Survival")
      }
    } else {
      pl <- pl + ylab(paste0("Mean (", kmmc, ")"))
    }
  }
  if(plot) {
    print(pl)
  }
  return(pl)
}