#' VPC function for survival-type data 
#' 
#' Creates a VPC plot from observed and simulation survival data
#' @param sim a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param obs a data.frame with observed data, containing the indenpendent and dependent variable, a column indicating the individual, and possibly covariates. E.g. load in from NONMEM using \link{read_table_nm}
#' @param rtte repeated time-to-event data? Deafult is FALSE (single-event TTE)
#' @param events numeric vector describing which events to show a VPC for when repeated TTE data, e.g. c(1:4). Default is NULL, which shows all events. 
#' @param pi_med plot the median of the simulated data? Default is FALSE.
#' @param obs_dv variable in data.frame for observed dependent value. "dv" by default
#' @param sim_dv variable in data.frame for simulated dependent value. "sdv" by default
#' @param obs_idv variable in data.frame for observed independent value. "time" by default
#' @param sim_idv variable in data.frame for simulated independent value. "time" by default
#' @param obs_id variable in data.frame for observed individual. "id" by default
#' @param sim_id variable in data.frame for simulated individual. "id" by default
#' @param nonmem should variable names standard to NONMEM be used (i.e. ID, TIME, DV, PRED). Default is "auto" for autodetect.
#' @param stratify character vector of stratification variables. Only 1 or 2 stratification variables can be supplied. If stratify_color is also specified, only 1 additional stratification can be specified.
#' @param stratify_color variable to stratify and color lines for observed data. Only 1 stratification variables can be supplied.
#' @param ci confidence interval to plot. Default is (0.05, 0.95)
#' @param plot Boolean indacting whether to plot the ggplot2 object after creation. Default is FALSE.
#' @param xlab ylab as numeric vector of size 2
#' @param ylab ylab as numeric vector of size 2
#' @param title title
#' @param smooth "smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.
#' @param theme which theme to load from the themes object
#' @param custom_theme specify a custom ggplot2 theme
#' @param facet either "wrap", "columns", or "rows" 
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
                    rtte = FALSE,
                    events = NULL,
                    obs_dv = "dv",
                    sim_dv =  "sdv",
                    obs_idv = "time",
                    sim_idv = "time",
                    obs_id = "id",
                    sim_id = "id",
                    pi_med = FALSE, 
                    nonmem = "auto",
                    stratify = NULL,
                    stratify_color = NULL,
                    legend_pos = NULL,
                    ci = c(0.05, 0.95),
                    plot = FALSE,
                    xlab = NULL, 
                    ylab = NULL,
                    title = NULL,
                    smooth = FALSE,
                    theme = "default",
                    custom_theme = NULL,
                    facet = "wrap") {
  if (nonmem == "auto") {
    if(sum(c("ID","TIME") %in% colnames(obs)) == 2) { # most likely, data is from NONMEM
      nonmem <- TRUE
    } else {
      nonmem <- FALSE
    }        
  } else {
    if(class(nonmem) != "logical") {
      nonmem <- FALSE
    }
  } 
  if(is.null(obs) && is.null(sim)) {
    stop("At least a simulation or an observation dataset are required to create a plot!")
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
  if (nonmem) {
    obs_dv = "dv"
    obs_idv = "time"
    obs_id = "id"
    sim_dv = "dv"
    sim_idv = "time"
    sim_id = "id"
    rtte_flag <- "rtte"
    if(!is.null(obs)) {
      if("MDV" %in% colnames(obs)) {
        obs <- obs[obs$MDV == 0,]
      }
      if("EVID" %in% colnames(obs)) {
        obs <- obs[obs$EVID == 0,]
      }      
    }
    if(!is.null(sim)) {
      if("MDV" %in% colnames(sim)) {
        sim <- sim[sim$MDV == 0,]
      }
      if("EVID" %in% colnames(obs)) {
        sim <- sim[sim$EVID == 0,]
      }
    }
    colnames(obs) <- tolower(colnames(obs))
    colnames(sim) <- tolower(colnames(sim))
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

  # format obs data
  if(!is.null(obs)) {
    if (length(obs[[obs_id]]) == 0) {
      warning("Warning: no ID column found, assuming 1 row per ID!")
      obs$id <- 1:length(obs[,1])
    }
    obs$time <- obs[[obs_idv]]
    obs$dv <- obs[[obs_dv]]
    obs <- relative_times(obs)
    if (rtte) {
      obs <- relative_times(obs)    
      obs <- obs %>% group_by(id) %>% mutate(rtte = cumsum(dv != 0)) 
      stratify <- c(stratify, "rtte")
    } else {
      obs$rtte <- 1
    }
  
    # add stratification column and comput KM curve for observations
    obs <- add_stratification(obs, stratify)
    obs_km <- compute_kaplan(obs, strat = "strat")

    # get bins
    obs_strat <- as.character(unique(obs$strat))
    bins <- list() 
    for (i in seq(obs_strat)) {
      bins[[obs_strat[i]]] <- sort(unique(obs[as.character(obs$strat) == obs_strat[i],]$time))
    }    
  } else { # get bins from sim
    obs_strat <- as.character(unique(sim$strat))
    bins <- list() 
    for (i in seq(obs_strat)) {
      bins[[obs_strat[i]]] <- sort(unique(sim[as.character(sim$strat) == obs_strat[i],]$time))
    }    
    obs_km <- NULL
  }
    
  if(!is.null(sim)) {
    # format sim data and compute KM curve CI for simulations
    sim$time <- sim[[sim_idv]]
    sim$sim <- add_sim_index_number(sim)
    n_sim <- length(unique(sim$sim))    
    all <- c()
    for (i in 1:n_sim) {
      tmp <- sim %>%
        filter(sim == i) %>%
        convert_from_dense_grid(.)   ## convert the simulation dataset to hold only the observations, not the whole grid
      if (rtte) {
        tmp <- tmp %>% group_by(id) %>% mutate(rtte = cumsum(dv != 0))       
      }
      tmp2 <- add_stratification(tmp %>% arrange(id, time), stratify)
      tmp3 <- compute_kaplan(tmp2, strat = "strat")
      tmp3[,c("bin", "bin_min", "bin_max", "bin_mid")] <- 0 
      for (j in seq(obs_strat)) {
        tmp3_spl <- tmp3[tmp3$strat == obs_strat[j],]
        if (length(tmp3_spl[,1]) > 0) {
          tmp_bins <- unique(c(0, bins[[obs_strat[j]]], max(tmp3_spl$time)))
          tmp3[tmp3$strat == obs_strat[j],] <- within(tmp3_spl, {
            bin <- cut(time, breaks = tmp_bins, labels = FALSE, right = TRUE)
            bin_min <- tmp_bins[bin] 
            bin_max <- tmp_bins[bin+1] 
            bin_mid <- (bin_min + bin_max) / 2
          })        
        }
      }
      all <- rbind(all, cbind(i, tmp3))
    }
  
    sim_km <- all %>% 
      group_by (bin, strat) %>% 
      summarise (bin_mid = head(bin_mid,1), bin_min = head(bin_min,1), bin_max = head(bin_max,1), 
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
    pl <- ggplot(sim_km, aes(x=bin_mid, y=qmed, group=strat))       
    if (smooth) {
      if (!is.null(stratify_color)) {
        pl <- pl + 
          geom_ribbon(aes(min = qmin, max=qmax, y=qmed, fill=strat_color), alpha=themes[[theme]]$med_area_alpha) +
          scale_fill_discrete(name="")
      } else {
        pl <- pl + geom_ribbon(aes(min = qmin, max=qmax, y=qmed), fill = themes[[theme]]$med_area, alpha=themes[[theme]]$med_area_alpha)        
      }
    } else {
      if (!is.null(stratify_color)) {
        pl <- pl + 
          geom_rect(aes(xmin=bin_min, xmax=bin_max, ymin=qmin, ymax=qmax, fill=strat_color), alpha=themes[[theme]]$med_area_alpha) +
          scale_fill_discrete(name="")
      } else {
        pl <- pl + geom_rect(aes(xmin=bin_min, xmax=bin_max, ymin=qmin, ymax=qmax), alpha=themes[[theme]]$med_area_alpha, fill = themes[[theme]]$med_area)           
      }
    }
    if (pi_med) {
      pl <- pl + geom_line_custom(linetype="dashed")
    }
  } else {
    pl <- pl + geom_step(data = obs_km)
  }
#   } else {
#     if (!is.null(stratify_color)) {
#       if (length(stratify) == 2) {
#         pl <- ggplot(obs_km, aes(x=time, y=dv, colour=strat2))         
#       } else {
#         pl <- ggplot(obs_km, aes(x=time, y=dv, colour=strat))           
#       }
#       pl <- pl + scale_colour_discrete(name=paste(stratify))
# #        theme(legend.title=element_blank())
#     } else {
#       pl <- ggplot(obs_km, aes(x=time, y=dv, group=strat, colour=strat))   
#     }
#   }
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
      chk_tbl <- obs_km %>% group_by(strat) %>% summarise(t = length(time))
      if (sum(chk_tbl$t <= 1)>0) { # it is not safe to use geom_step, so use 
        geom_step <- geom_line
      }
      warning ("Warning, some strata in the observed data had zero or one observations, using line instead of step plot. Consider using less strata (e.g. using the 'events' argument).")
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
    if (length(stratify_original) == 1) {
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
  if (!is.null(title)) {
    pl <- pl + ggtitle(title)  
  }
  if (!is.null(custom_theme)) {  
    pl <- pl + custom_theme()    
  } else {
    if (!is.null(theme)) {
      pl <- pl + theme_plain()
    } 
  }
  # place legend in better spot
  if(!is.null(legend_pos)) {
    pl <- pl + theme(legend.position = legend_pos)    
  }
  if(!is.null(xlab)) {
    pl <- pl + xlab(xlab)
  } else {
    pl <- pl + xlab("Time")
  }
  if(!is.null(ylab)) {
    pl <- pl + ylab(ylab)
  } else {
    pl <- pl + ylab("Survival (%)")
  }
  if(plot) {
    print(pl)
  }
  return(pl)
}