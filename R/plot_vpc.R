plot_vpc <- function(db) {
  with(db, {
    if (!is.null(db$sim)) {
      pl <- ggplot(db$vpc_dat, aes(x=bin_mid)) 
      if(show$sim_median) {
        pl <- pl + geom_line(aes(y=q50.med), colour=db$vpc_theme$sim_median_color, linetype=vpc_theme$sim_median_linetype, size=vpc_theme$sim_median_size)           
      }
      if(show$pi_as_area) {
        if (smooth) {
          pl <- pl +
            geom_ribbon(aes(x=bin_mid, y=q50.med, ymin=q5.med, ymax=q95.med), alpha=vpc_theme$sim_median_alpha, fill = vpc_theme$sim_median_fill) 
        } else {
          pl <- pl +
            geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q50.med, ymin=q5.med, ymax=q95.med), alpha=vpc_theme$sim_median_alpha, fill = vpc_theme$sim_median_color) 
        }       
      } else {
        if(show$sim_median_ci) {
          if (smooth) {
            pl <- pl +
              geom_ribbon(aes(x=bin_mid, y=q50.low, ymin=q50.low, ymax=q50.up), alpha=vpc_theme$sim_median_alpha, fill = vpc_theme$sim_median_fill) 
          } else {
            pl <- pl +
              geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q50.low, ymin=q50.low, ymax=q50.up), alpha=vpc_theme$sim_median_alpha, fill = vpc_theme$sim_median_color) 
          }       
        }
        if (show$pi) {
          pl <- pl + 
            geom_line(aes(x=bin_mid, y=q5.med), colour=vpc_theme$sim_pi_color, linetype=vpc_theme$sim_pi_linetype, size=vpc_theme$sim_pi_size) +
            geom_line(aes(x=bin_mid, y=q95.med), colour=vpc_theme$sim_pi_color, linetype=vpc_theme$sim_pi_linetype, size=vpc_theme$sim_pi_size)       
        }
        if (show$pi_ci) {
          if (smooth) {
            pl <- pl + 
              geom_ribbon(aes(x=bin_mid, y=q5.low, ymin=q5.low, ymax=q5.up), alpha=vpc_theme$sim_pi_alpha, fill = vpc_theme$sim_pi_fill) +
              geom_ribbon(aes(x=bin_mid, y=q95.low, ymin=q95.low, ymax=q95.up), alpha=vpc_theme$sim_pi_alpha, fill = vpc_theme$sim_pi_fill) 
          } else {
            pl <- pl + 
              geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q5.low, ymin=q5.low, ymax=q5.up), alpha=vpc_theme$sim_pi_alpha, fill = vpc_theme$sim_pi_fill) +
              geom_rect(aes(xmin=bin_min, xmax=bin_max, y=q95.low, ymin=q95.low, ymax=q95.up), alpha=vpc_theme$sim_pi_alpha, fill = vpc_theme$sim_pi_fill)     
          }      
        }      
      }
    } else {
      if (!is.null(stratify_color)) {
        if (length(stratify) == 2) {
          pl <- ggplot(aggr_obs, aes(colour=as.factor(strat2)))         
        } else {
          pl <- ggplot(aggr_obs, aes(colour=as.factor(strat)))           
        }
        pl <- pl + scale_colour_discrete(name="")
      } else {
        pl <- ggplot(aggr_obs)  
      }
    }
    if(!is.null(obs)) {
      if (show$obs_median) {
        pl <- pl +
          geom_line(data=aggr_obs, aes(x=bin_mid, y=obs50), linetype=vpc_theme$obs_median_linetype, colour=vpc_theme$obs_median_color, size=vpc_theme$obs_median_size)       
      }
      if(show$obs_ci) {
        pl <- pl +
          geom_line(data=aggr_obs, aes(x=bin_mid, y=obs5), linetype=vpc_theme$obs_ci_linetype, colour=vpc_theme$obs_ci_color, size=vpc_theme$obs_ci_size) +
          geom_line(data=aggr_obs, aes(x=bin_mid, y=obs95), linetype=vpc_theme$obs_ci_linetype, colour=vpc_theme$obs_ci_color, size=vpc_theme$obs_ci_size) 
      }
      if (show$obs_dv) {
        pl <- pl + geom_point(data=obs, aes(x=idv, y = dv), size=vpc_theme$obs_size, colour=vpc_theme$obs_color)
      }    
    }
    bdat <- data.frame(cbind(x=bins, y=NA))
    pl <- pl + 
      geom_rug(data=bdat, sides = "t", aes(x = x, y=y), colour=vpc_theme$bin_separators_color)
    pl <- pl + xlab(xlab) + ylab(ylab)
    if (log_y) {
      pl <- pl + scale_y_log10() 
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
      } else { # 2 grid-stratification 
        if ("strat1" %in% c(colnames(vpc_dat), colnames(aggr_obs))) {
          if(length(grep("row", facet))>0) {
            pl <- pl + facet_grid(strat1 ~ strat2)                
          } else {
            pl <- pl + facet_grid(strat2 ~ strat1)                
          }        
        } else { # only color stratification
          if ("strat" %in% c(colnames(vpc_dat), colnames(aggr_obs))) {
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
    if (!is.null(ggplot_theme)) {  
      pl <- pl + ggplot_theme()    
    } else {
      if (!is.null(theme)) {
        pl <- pl + theme_plain()
      } 
    }
    if (plot) {
      print(pl)    
    }
    return(pl)    
  })
}
