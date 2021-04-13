#' VPC plotting function
#'
#' This function performs no parsing of data, it just plots the already calculated statistics generated using one of the 
#' `vpc` functions.
#' 
#' @param db object created using the `vpc` function
#' @param show what to show in VPC (obs_dv, obs_ci, pi, pi_as_area, pi_ci, obs_median, sim_median, sim_median_ci)
#' @param vpc_theme theme to be used in VPC. Expects list of class vpc_theme created with function vpc_theme()
#' @param smooth "smooth" the VPC (connect bin midpoints) or show bins as rectangular boxes. Default is TRUE.
#' @param log_x Boolean indicting whether x-axis should be shown as logarithmic. Default is FALSE.
#' @param log_y Boolean indicting whether y-axis should be shown as logarithmic. Default is FALSE.
#' @param title title
#' @param xlab label for x axis 
#' @param ylab label for y axis
#' @param verbose show debugging information (TRUE or FALSE)
#' @export
#' @seealso \link{sim_data}, \link{vpc_cens}, \link{vpc_tte}, \link{vpc_cat}
#' @examples 
#' ## See vpc.ronkeizer.com for more documentation and examples
#' 
#' library(vpc)
#' vpc_db <- vpc(sim = simple_data$sim, obs = simple_data$obs, vpcdb = TRUE)
#' plot_vpc(vpc_db, title = "My new vpc", x = "Custom x label")
plot_vpc <- function(db,
                     show = NULL,
                     vpc_theme = NULL,
                     smooth = TRUE,
                     log_x = FALSE,
                     log_y = FALSE,
                     xlab = NULL,
                     ylab = NULL,
                     title = NULL,
                     verbose = FALSE) {
  if(is.null(vpc_theme) || (class(vpc_theme) != "vpc_theme")) {
    vpc_theme <- new_vpc_theme()
  }
  # Setup show using first the defaults, then the items from the db, then the argument
  show_default_current <- show_default[[db$type]]
  if (is.null(show_default_current)) {
    stop("Invalid 'type' of `db`: ", db$type)
  }
  show_db <- replace_list_elements(show_default_current, db$show)
  show <- replace_list_elements(show_db, show)

  plot_vpc_helper(
    db=db,
    show=show,
    vpc_theme=vpc_theme,
    smooth=smooth,
    log_x=log_x,
    log_y=log_y,
    xlab=xlab,
    ylab=ylab,
    title=title,
    verbose=verbose
  )
}

#' Helper function to simplify vpc plotting (not intended to be called directly)
#' 
#' @inheritParams plot_vpc
#' @keywords internal
#' @export
plot_vpc_helper <- function(db,
                            show = NULL,
                            vpc_theme = NULL,
                            smooth = TRUE,
                            log_x = FALSE,
                            log_y = FALSE,
                            xlab = NULL,
                            ylab = NULL,
                            title = NULL,
                            verbose = FALSE) {
  UseMethod("plot_vpc_helper")
}

#' @rdname plot_vpc_helper
#' @export
plot_vpc_helper.default <- function(db, ...) {
  stop("No method to plot_vpc helper for class: ", paste(class(db), collapse=", "))
}

#' @rdname plot_vpc_helper
#' @export
plot_vpc_helper.vpcdb_continuous <- function(db, ...,
                                             show = NULL,
                                             vpc_theme = NULL,
                                             smooth = TRUE,
                                             log_x = FALSE,
                                             log_y = FALSE,
                                             xlab = NULL,
                                             ylab = NULL,
                                             title = NULL,
                                             verbose = FALSE) {
  if(!is.null(db$stratify)) {
    ## rename "strat" to original stratification variable names
    if(length(db$stratify) == 1) {
      if(!is.null(db$aggr_obs)) colnames(db$aggr_obs)[match("strat", colnames(db$aggr_obs))] <- db$stratify[1]
      if(!is.null(db$vpc_dat)) colnames(db$vpc_dat)[match("strat", colnames(db$vpc_dat))] <- db$stratify[1]
    }
    if(length(db$stratify) == 2) {
      if(!is.null(db$aggr_obs)) {
        colnames(db$aggr_obs)[match("strat1", colnames(db$aggr_obs))] <- db$stratify[1]
        colnames(db$aggr_obs)[match("strat2", colnames(db$aggr_obs))] <- db$stratify[2]
      }
      if(!is.null(db$vpc_dat)) {
        colnames(db$vpc_dat)[match("strat1", colnames(db$vpc_dat))] <- db$stratify[1]
        colnames(db$vpc_dat)[match("strat2", colnames(db$vpc_dat))] <- db$stratify[2]
      }
    }
  }
  if (!is.null(db$sim) && is.factor(db$vpc_dat$bin)) {
    db$vpc_dat$bin_mid <- db$vpc_dat$bin
  }
  if (!is.null(db$obs) && !is.null(db$vpc_dat) && is.factor(db$vpc_dat$bin)) {
    db$aggr_obs$bin_mid <- db$aggr_obs$bin
  }
  
  pl <-
    ggplot2::ggplot(data=db) +
    geom_sim_median_continuous(data=db, show=show$sim_median, vpc_theme=vpc_theme) +
    geom_sim_pi_as_area_continuous(data=db, show=show$pi_as_area, smooth=smooth, vpc_theme=vpc_theme) +
    # The next several items are only shown if !show$pi_as_area to prevent
    # representing the same data multiple times.
    geom_sim_median_ci(data=db, show=!show$pi_as_area & show$sim_median_ci, vpc_theme=vpc_theme) +
    geom_sim_pi_continuous(data=db, show=!show$pi_as_area & show$pi, vpc_theme=vpc_theme) +
    # Since you can't have multiple geoms added together without a plot, the
    # lower and upper bounds are called twice with the `what` argument.
    geom_sim_pi_ci_continuous(
      data=db,
      show=!show$pi_as_area & show$pi_ci,
      smooth=smooth,
      vpc_theme=vpc_theme,
      what="q5"
    ) +
    geom_sim_pi_ci_continuous(
      data=db,
      show=!show$pi_as_area & show$pi_ci,
      smooth=smooth,
      vpc_theme=vpc_theme,
      what="q95"
    ) +
    geom_obs_median_continuous(data=db, show=show$obs_median, vpc_theme=vpc_theme) +
    geom_obs_ci_continuous(data=db, show=show$obs_ci, vpc_theme=vpc_theme) +
    geom_obs_dv_continuous(data=db, show=show$obs_dv, vpc_theme=vpc_theme) +
    geom_bin_sep(bins=db$bins, show=show$bin_sep, vpc_theme=vpc_theme) +
    ggplot2::labs(
      # While title is not always used, it defaults to NULL which causes no
      # title to display.
      title=title,
      x=ifelse(!is.null(xlab), xlab, db$xlab),
      y=ifelse(!is.null(ylab), ylab, db$ylab)
    ) +
    facet_continuous(data=db) +
    scale_x_log10_vpc(data=db, show=log_x) +
    scale_y_log10_vpc(log_y) +
    geom_hline_loq(data=db, vpc_theme=vpc_theme) +
    theme_plain()
  return(pl)
}

#' @rdname plot_vpc_helper
#' @export
plot_vpc_helper.vpcdb_categorical <- function(db, ...) {
  plot_vpc_helper.vpcdb_continuous(db=db, ...)
}

#' @rdname plot_vpc_helper
#' @export
plot_vpc_helper.vpcdb_censored <- function(db, ...) {
  plot_vpc_helper.vpcdb_continuous(db=db, ...)
}

#' @rdname plot_vpc_helper
#' @export
plot_vpc_helper.vpcdb_time_to_event <- function(db, ...,
                                                show = NULL,
                                                vpc_theme = NULL,
                                                smooth = TRUE,
                                                log_x = FALSE,
                                                log_y = FALSE,
                                                xlab = NULL,
                                                ylab = NULL,
                                                title = NULL,
                                                verbose = FALSE) {
  # Prepare the database
  if(!is.null(db$stratify_pars)) {
    ## rename "strat" to original stratification variable names
    if(length(db$stratify_pars) == 1) {
      # "strat1" ==> "rtte"
      if(!is.null(db$obs_km)) db$obs_km[[db$stratify_pars[1]]] <- as.factor(db$obs_km$strat)
      if(!is.null(db$sim_km)) db$sim_km[[db$stratify_pars[1]]] <- as.factor(db$sim_km$strat)
      if(!is.null(db$all_dat)) db$all_dat[[db$stratify_pars[1]]] <- as.factor(db$all_dat$strat)
    }
    if(length(db$stratify_pars) == 2) {
      if(!is.null(db$obs_km)) {
        db$obs_km[[db$stratify_pars[1]]] <- as.factor(db$obs_km$strat1)
        db$obs_km[[db$stratify_pars[2]]] <- as.factor(db$obs_km$strat2)
      }
      if(!is.null(db$sim_km)) {
        db$sim_km[[db$stratify_pars[1]]] <- as.factor(db$sim_km$strat1)
        db$sim_km[[db$stratify_pars[2]]] <- as.factor(db$sim_km$strat2)
      }
    }
  }
  if(!is.null(db$obs_km)) db$obs_km$bin_mid <- c(0, diff(db$obs_km$time))
  
  show$pi_as_area <- TRUE
  pl <-
    ggplot2::ggplot(data=db, mapping=ggplot2::aes(x=bin_mid, y=qmed)) +
    geom_sim_km(data=db, show=show$sim_km) +
    geom_sim_pi_as_area_tte(data=db, show=show$pi_as_area, smooth=smooth, vpc_theme=vpc_theme) +
    geom_obs_cens_dat_tte(data=db) +
    geom_sim_median_tte(data=db, show=show$sim_median, smooth=smooth) +
    geom_obs_ci_tte(data=db, show=show$obs_ci, vpc_theme=vpc_theme) +
    geom_obs_km(data=db) +
    facet_tte(data=db) +
    geom_bin_sep(bins=db$tmp_bins, show=show$bin_sep, vpc_theme=vpc_theme) +
    guides_stratify_color(data=db) +
    ggplot2::labs(
      # TODO: Review 2021-04: `title` was previously ignored.  It will now cause
      # a title to be shown.

      # While title is not always used, it defaults to NULL which causes no
      # title to display.
      title=title,
      x=ifelse(!is.null(xlab), xlab, db$xlab),
      y=ifelse(!is.null(ylab), ylab, db$ylab)
    ) +
    # TODO: Review 2021-04: added theme_plain() to align with continuous
    # plotting
    theme_plain()
  return(pl)
}

#' Create a ggplot for each vpcdb type
#' 
#' These functions are not to be called directly by users; they are for internal
#' use.  Users should call \code{plot_vpc()}.
#' 
#' @export
ggplot.vpcdb_continuous <- function(data = NULL, mapping = NULL, ..., environment = parent.frame()) {
  if (!is.null(data$sim)) {
    pl <- ggplot2::ggplot(data$vpc_dat, ggplot2::aes(x=bin_mid, group=1))
  } else {
    pl <- ggplot2::ggplot(data$aggr_obs)
  }
  pl
}

#' @rdname ggplot.vpcdb_continuous
#' @export
ggplot.vpcdb_categorical <- function(data = NULL, mapping = NULL, ..., environment = parent.frame()) {
  ggplot.vpcdb_continuous(data=data, mapping=mapping, ..., environment=environment)
}

#' @rdname ggplot.vpcdb_continuous
#' @export
ggplot.vpcdb_censored <- function(data = NULL, mapping = NULL, ..., environment = parent.frame()) {
  ggplot.vpcdb_continuous(data=data, mapping=mapping, ..., environment=environment)
}

#' @rdname ggplot.vpcdb_continuous
#' @export
ggplot.vpcdb_time_to_event <- function(data = NULL, mapping = NULL, ..., environment = parent.frame()) {
  if(!is.null(data$sim_km)) {
    if (is.null(mapping)) {
      mapping <- ggplot2::aes(x=bin_mid, y=qmed)
    }
    pl <- ggplot2::ggplot(data=data$sim_km, mapping=mapping)
  } else if (!is.null(data$obs_km)) {
    if (is.null(mapping)) {
      mapping <- ggplot2::aes()
    }
    pl <- ggplot2::ggplot(data=data$obs_km, mapping=mapping)
  } else {
    stop("For time-to-event, either sim_km or obs_km must be available")
  }
  pl
}

#' A collection of internal ggplot helpers for VPC plotting
#' 
#' @param data The vpcdb object
#' @param show Should the geom be shown? (TRUE/FALSE)
#' @param vpc_theme The theme to use
#' @name vpc_ggplot
NULL

#' @describeIn vpc_ggplot Show rug plot of bin separators
#' @param bins Numeric vector of bin separators (if logical value, geom_blank is
#'   returned)
geom_bin_sep <- function(bins, show, vpc_theme) {
  ret <- ggplot2::geom_blank()
  if (show) {
    if(!(class(bins) == "logical" && bins == FALSE)) {
      bdat <- data.frame(cbind(x = bins, y = NA))
      ret <-
        ggplot2::geom_rug(
          data=bdat,
          sides = "t",
          ggplot2::aes(x = x, y = y, group=NA),
          colour=vpc_theme$bin_separators_color
        )
    }
  }
  ret
}

#' @describeIn vpc_ggplot Generate hlines for the lloq/uloq
geom_hline_loq <- function(data, vpc_theme) {
  loq_values <- c(data$lloq, data$uloq)
  if(!is.null(loq_values)) {
    ggplot2::geom_hline(yintercept = loq_values, colour=vpc_theme$loq_color) 
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show the cens_dat for time-to-events
geom_obs_cens_dat_tte <- function(data) {
  if(!is.null(data$cens_dat) && nrow(data$cens_dat)>0) {
    ggplot2::geom_point(
      data=data$cens_dat, 
      ggplot2::aes(x=time, y = y),
      shape="|",
      size=2.5
    )
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show confidence interval for observed, continuous data
geom_obs_ci_continuous <- function(data, show, vpc_theme) {
  if (show & !is.null(data$obs) && !is.null(data$aggr_obs[["obs5"]])) {
    # TODO: Review 2021-04: Changed to geom_ribbon so that it could be a single
    # geom (since you can't add two geoms together without a plot)
    ggplot2::geom_ribbon(
      data=data$aggr_obs,
      ggplot2::aes(x=bin_mid, ymin=obs5, ymax=obs95),
      colour=vpc_theme$obs_ci_color,
      fill=NA,
      linetype=vpc_theme$obs_ci_linetype,
      size=vpc_theme$obs_ci_size,
      outline.type="both"
    )
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show confidence interval for observed, time-to-event
#'   data
geom_obs_ci_tte <- function(data, show, vpc_theme) {
  if(show & !is.null(data$obs)) {
    ggplot2::geom_ribbon(
      data=data$obs_km, 
      ggplot2::aes(x=time, ymin=lower, ymax=upper, group=strat), 
      fill=vpc_theme$obs_ci_fill,
      colour = NA
    )
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show observed data points
geom_obs_dv_continuous <- function(data, show, vpc_theme) {
  if(show & !is.null(data$obs)) {
    ggplot2::geom_point(
      data=data$obs,
      ggplot2::aes(x=idv, y = dv),
      size=vpc_theme$obs_size,
      colour=vpc_theme$obs_color,
      alpha = vpc_theme$obs_alpha,
      shape = vpc_theme$obs_shape
    )
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show observed Kaplan-Meier line
geom_obs_km <- function(data) {
  if (!is.null(data$obs)) {
    chk_tbl <-
      data$obs_km %>%
      dplyr::group_by(strat) %>%
      dplyr::summarise(t = length(time))
    if (sum(chk_tbl$t <= 1)>0) { # it is not safe to use geom_step, so use
      # TODO: Review 2021-04: The way that the code was structured before,
      # geom_line was never used.  Updated to conditionally use geom_line.
      warning("Some strata in the observed data had zero or one observations, using line instead of step plot. Consider using fewer strata (e.g. using the 'events' argument).")
      geom_fun <- ggplot2::geom_line
    } else {
      geom_fun <- ggplot2::geom_step
    }
    if(!is.null(data$stratify_color)) {
      ret <-
        geom_fun(
          data = data$obs_km, 
          ggplot2::aes(x=time, y=surv, colour=get(data$stratify_color[1])),
          size=.8
        )
    } else {
      ret <-
        geom_fun(
          data = data$obs_km, 
          ggplot2::aes(x=time, y=surv, group=strat),
          size=.8
        )
    }
  } else {
    ret <- ggplot2::geom_blank()
  }
  ret
}

#' @describeIn vpc_ggplot Show observed median line for continuous data
geom_obs_median_continuous <- function(data, show, vpc_theme) {
  if (show & !is.null(data$aggr_obs)) {
    ggplot2::geom_line(
      data=data$aggr_obs,
      ggplot2::aes(x=bin_mid, y=obs50), 
      linetype=vpc_theme$obs_median_linetype, 
      colour=vpc_theme$obs_median_color, 
      size=vpc_theme$obs_median_size
    )
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show simulated Kaplan-Meier curves for time-to-event
#'   data
geom_sim_km <- function(data, show) {
  if (show) {
    data$all_dat$strat_sim <- paste0(data$all_dat$strat, "_", data$all_dat$i)
    transp <- min(.1, 20*(1/length(unique(data$all_dat$i))))
    ggplot2::geom_step(
      data = data$all_dat,
      mapping = ggplot2::aes(x=bin_mid, y=surv, group=strat_sim),
      colour=grDevices::rgb(0.2,.53,0.796, transp)
    )
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show simulated median line for continuous data
geom_sim_median_continuous <- function(data, show, vpc_theme) {
  if (show & !is.null(data$sim)) {
    ggplot2::geom_line(
      ggplot2::aes(y=q50.med),
      colour=vpc_theme$sim_median_color,
      linetype=vpc_theme$sim_median_linetype,
      size=vpc_theme$sim_median_size
    )
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show simulated median line (or steps) for
#'   time-to-event data
geom_sim_median_tte <- function(data, show, smooth) {
  if (show & !is.null(data$sim_km)) {
    if (smooth) {
      geom_line_custom <- ggplot2::geom_line
    } else {
      geom_line_custom <- ggplot2::geom_step
    }
    geom_line_custom(linetype="dashed")
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show simulated median confidence interval for
#'   continuous data
geom_sim_median_ci_continuous <- function(data, show, smooth, vpc_theme) {
  if (show & !is.null(data$sim)) {
    if (smooth) {
      ggplot2::geom_ribbon(ggplot2::aes(x=bin_mid, ymin=q50.low, ymax=q50.up), alpha=vpc_theme$sim_median_alpha, fill = vpc_theme$sim_median_fill)
    } else {
      ggplot2::geom_rect(ggplot2::aes(xmin=bin_min, xmax=bin_max, ymin=q50.low, ymax=q50.up), alpha=vpc_theme$sim_median_alpha, fill = vpc_theme$sim_median_fill)
    }
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show simulated prediction interval area for continuous
#'   data
geom_sim_pi_as_area_continuous <- function(data, show, smooth, vpc_theme) {
  if (show & !is.null(data$sim)) {
    if (smooth) {
      ggplot2::geom_ribbon(
        ggplot2::aes(x=bin_mid, ymin=q5.med, ymax=q95.med),
        alpha=vpc_theme$sim_median_alpha,
        fill = vpc_theme$sim_median_fill
      )
    } else {
      ggplot2::geom_rect(
        ggplot2::aes(xmin=bin_min, xmax=bin_max, ymin=q5.med, ymax=q95.med),
        alpha=vpc_theme$sim_median_alpha,
        fill = vpc_theme$sim_median_fill
      )
    }
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show simulated prediction interval area for
#'   time-to-event data
geom_sim_pi_as_area_tte <- function(data, show, smooth, vpc_theme) {
  if (show & !is.null(data$sim_km)) {
    if(smooth) {
      if(!is.null(data$stratify_color)) {
        ret <-
          ggplot2::geom_ribbon(
            data = data$sim_km, 
            ggplot2::aes(min = qmin, max=qmax, fill = get(data$stratify_color[1])), 
            alpha=vpc_theme$sim_median_alpha
          )
      } else {
        ret <-
          ggplot2::geom_ribbon(
            data = data$sim_km, 
            ggplot2::aes(ymin = qmin, ymax=qmax), 
            fill = vpc_theme$sim_median_fill, 
            alpha=vpc_theme$sim_median_alpha
          )
      }
    } else {
      if(!is.null(data$stratify_color)) {
        ret <-
          ggplot2::geom_rect(
            data = data$sim_km, 
            ggplot2::aes(xmin=bin_min, xmax=bin_max, ymin=qmin, ymax=qmax, fill = get(data$stratify_color[1])), 
            alpha=vpc_theme$sim_median_alpha
          )
      } else {
        ret <-
          ggplot2::geom_rect(
            data = data$sim_km, 
            ggplot2::aes(xmin=bin_min, xmax=bin_max, ymin=qmin, ymax=qmax), 
            alpha=vpc_theme$sim_median_alpha, 
            fill = vpc_theme$sim_median_fill
          )
      }
    }
  } else {
    ret <- ggplot2::geom_blank()
  }
  ret
}

#' @describeIn vpc_ggplot Show simulated prediction interval lines for
#'   continuous data
geom_sim_pi_continuous <- function(data, show, vpc_theme) {
  if (show & !is.null(data$sim)) {
    # TODO: Review 2021-04: Changed to geom_ribbon so that it could be a single
    # geom (since you can't add two geoms together without a plot)
    ggplot2::geom_ribbon(
      ggplot2::aes(x=bin_mid, ymin=q5.med, ymax=q95.med),
      colour=vpc_theme$sim_pi_color,
      fill=NA,
      linetype=vpc_theme$sim_pi_linetype,
      size=vpc_theme$sim_pi_size,
      outline.type="both"
    )
  } else {
    ggplot2::geom_blank()
  }
}

#' @describeIn vpc_ggplot Show simulated prediction interval confidence interval
#'   areas for continuous data
geom_sim_pi_ci_continuous <- function(data, show, vpc_theme, what=c("q5", "q95")) {
  what <- match.arg(what)
  ret <- geom_blank()
  if (show && !is.null(data$sim) && ("q5.low" %in% names(data$vpc_dat))) {
    if (smooth) {
      mapping <-
        if (what == "q5") {
          ggplot2::aes(x=bin_mid, ymin=q5.low, ymax=q5.up)
        } else if (what == "q95") {
          ggplot2::aes(x=bin_mid, ymin=q95.low, ymax=q95.up)
        } else {
          stop("Invalid value for `what`") # nocov
        }
      ret <-
        ggplot2::geom_ribbon(
          mapping=mapping,
          alpha=vpc_theme$sim_pi_alpha,
          fill = vpc_theme$sim_pi_fill
        )
    } else {
      mapping <-
        if (what == "q5") {
          ggplot2::aes(xmin=bin_min, xmax=bin_max, y=q5.low, ymin=q5.low, ymax=q5.up)
        } else if (what == "q95") {
          ggplot2::aes(xmin=bin_min, xmax=bin_max, y=q95.low, ymin=q95.low, ymax=q95.up)
        } else {
          stop("Invalid value for `what`") # nocov
        }
      ret <-
        ggplot2::geom_rect(
          mapping=mapping,
          alpha=vpc_theme$sim_pi_alpha,
          fill = vpc_theme$sim_pi_fill
        )
    }
  }
  ret
}

#' @describeIn vpc_ggplot Facet continuous data by stratification factors
facet_continuous <- function(data) {
  ret <- ggplot2::facet_null()
  if (!is.null(data$stratify)) {
    if (is.null(data$labeller)) data$labeller <- ggplot2::label_both
    if (length(data$stratify) == 1) {
      if (data$facet == "wrap") {
        ret <-
          ggplot2::facet_wrap(
            stats::reformulate(data$stratify[1], NULL),
            scales = data$scales, 
            labeller = data$labeller
          )
      } else if (length(grep("row", data$facet))>0) {
        ret <-
          ggplot2::facet_grid(
            stats::reformulate(data$stratify[1], NULL),
            scales = data$scales,
            labeller = data$labeller
          )
      } else {
        ret <-
          ggplot2::facet_grid(
            stats::reformulate(".", data$stratify[1]),
            scales = data$scales,
            labeller = data$labeller
          )
      }
    } else { # 2 grid-stratification
      if (data$stratify[1] %in% c(colnames(data$vpc_dat), colnames(data$aggr_obs))) {
        if(length(grep("row", data$facet))>0) {
          ret <-
            ggplot2::facet_grid(
              stats::reformulate(data$stratify[1], data$stratify[2]),
              scales = data$scales,
              labeller = data$labeller
            )
        } else {
          ret <-
            ggplot2::facet_grid(
              stats::reformulate(data$stratify[2], data$stratify[1]),
              scales = data$scales,
              labeller = data$labeller
            )
        }
      } else if ("strat" %in% c(colnames(data$vpc_dat), colnames(data$aggr_obs))) {
        # color stratification only
      } else {
        stop ("Stratification unsuccesful.")
      }
    }
  }
  ret
}

#' @describeIn vpc_ggplot Facet time-to-event data by stratification factors
facet_tte <- function(data) {
  # Default to a single panel
  ret <- ggplot2::facet_null()
  # Or, choose something more complex
  if (!is.null(data$stratify) || data$rtte) {
    if (is.null(data$labeller)) {
      data$labeller <- label_vpc_tte
    }
    if (length(data$stratify_pars) == 1 | data$rtte) {
      if (data$facet == "wrap") {
        ret <-
          ggplot2::facet_wrap(
            stats::reformulate(data$stratify_pars[1], NULL),
            scales = data$scales,
            labeller = data$labeller
          )
      } else if (length(grep("row", data$facet)) > 0) {
        ret <-
          ggplot2::facet_grid(
            stats::reformulate(data$stratify_pars[1], NULL),
            scales = data$scales,
            labeller = data$labeller
          )
      } else {
        ret <-
          ggplot2::facet_grid(
            stats::reformulate(".", data$stratify_pars[1]),
            scales = data$scales,
            labeller = data$labeller
          )
      }
    } else if (length(grep("row", data$facet)) > 0) {
      ret <-
        ggplot2::facet_grid(
          stats::reformulate(data$stratify_pars[1], data$stratify_pars[2]),
          scales = data$scales,
          labeller = data$labeller
        )
    } else {
      ret <-
        ggplot2::facet_grid(
          stats::reformulate(data$stratify_pars[2], data$stratify_pars[1]),
          scales = data$scales,
          labeller = data$labeller
        )
    }
  }
  ret
}

#' @describeIn vpc_ggplot Generate guides for stratification fill and colour
guides_stratify_color <- function(data) {
  if(!is.null(data$stratify_color)) {
    ret <-
      ggplot2::guides(
        fill = ggplot2::guide_legend(title=data$stratify_color[1]),
        colour = ggplot2::guide_legend(title=data$stratify_color[1])
      )
  } else {
    ret <- ggplot2::guides()
  }
  ret
}

#' @describeIn vpc_ggplot Optionally show log-x scale
scale_x_log10_vpc <- function(data, show) {
  ret <- ggplot2::scale_x_continuous()
  if (show) {
    if(!is.factor(data$vpc_dat$bin)) {
      ret <- ggplot2::scale_x_log10()
    }
    else {
      warning("log_x option has no effect when the IDV is a factor")
    }
  }
  ret
}

#' @describeIn vpc_ggplot Optionally show log-y scale
scale_y_log10_vpc <- function(show) {
  ret <- ggplot2::scale_y_continuous()
  if (show) {
    ret <- ggplot2::scale_y_log10()
  }
  ret
}
