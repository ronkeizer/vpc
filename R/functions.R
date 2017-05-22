#' Add noise / residual error to data
#'
#' @param x data
#' @param ruv list describing the magnitude of errors. List arguments: "proportional", "additive", "exponential".
#' @export
add_noise <- function(x, ruv = list(proportional = 0, additive = 0, exponential = 0)) {
  if (is.null(ruv$proportional)) { ruv$proportional <- 0 }
  if (is.null(ruv$additive)) { ruv$additive <- 0 }
  if (is.null(ruv$exponential)) { ruv$exponential <- 0 }
  x * (1 + rnorm(length(x), 0, ruv$proportional)) +  rnorm(length(x), 0, ruv$additive) * exp(rnorm(length(x), 0, ruv$exponential))
}

add_recurs <- function(x, n, max) {
  x <- x + n
  n <- n + 1
  if (n <= max) {
    x <- add_recurs(x, n, max)
  }
  x
}

locf <- function(S) {
  L <- !is.na(S)
  c(S[L][1], S[L])[cumsum(L)+1]
}

add_step <- function(dat = ., vars) {
    dat$step <- 0
    tmp <- dat[-1,]
    tmp$step <- 1
    tmp[,vars] <- dat[-length(dat[,1]), vars]
    newdat <- data.frame(rbind(dat, tmp))
    newdat %>% dplyr::arrange(bin, -step)
}

as.num <- function(x) { as.numeric(as.character(x)) }

convert_to_dense_grid <- function(dat, t = "t", id = "id", t_start = 0, t_step = 1, add = NULL) {
  t = seq(from=t_start, to=max(dat$t), by=t_step)
  tmp <- data.frame(cbind(id = rep(unique(dat$id), each = length(t)),
                          t  = rep(t, n = length(unique(dat$id))) ) )
  tmp$dv <- 0
  id_t <- paste0(dat$id, "-", dat$t)
  tmp[match(id_t, paste0(tmp$id,"-",tmp$t)),]$dv <- dat$dv
  tmp$rtte <- 0
  tmp[match(id_t, paste0(tmp$id,"-",tmp$t)),]$rtte <- 1
  if (!is.null(add)) {
    tmp2 <- merge(tmp, dat[,c("id", add)] %>% dplyr::group_by(id) %>% do(.[1,]), by = "id", all.y = FALSE)
  }
  return(tmp2)
}

relative_times <- function (dat, simulation = FALSE) {
  if (simulation) {
    tmp <- dat %>% dplyr::group_by(sim, id)
  } else {
    tmp <- dat %>% dplyr::group_by(id)
  }
  tmp2 <- tmp %>% dplyr::arrange(time) %>% dplyr::mutate(time = c(time[1], diff(time)))
  if (simulation) {
    return(tmp2 %>% dplyr::arrange(sim, id, time))
  } else {
    return(tmp2 %>% dplyr::arrange(id, time))
  }
}

convert_from_dense_grid <- function (dat) { # note: only for a single trial, requires a loop or ddply for multiple subproblems
  tmp <- dat %>% dplyr::group_by(id)
  if("rtte" %in% names(dat)) {
    tmp <- tmp %>% filter (rtte == 1)
  }
  #  filter (dv == 1 | time == max(time) )
  tmp2 <- rbind(tmp %>% filter(length(time) > 1) %>% mutate(time = time - c(0,time[1:(length(time)-1)])),
                tmp %>% filter(length(time) == 1) )
  return(tmp2 %>% dplyr::arrange(id, time))
}
