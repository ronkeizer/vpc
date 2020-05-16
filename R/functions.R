msg <- function(txt, verbose = FALSE) {
  if(verbose) {
    message(txt)
  }
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
    tmp2 <- merge(tmp, dat[,c("id", add)] %>% dplyr::group_by_("id") %>% dplyr::do(.[1,]), by = "id", all.y = FALSE)
  }
  return(tmp2)
}

relative_times <- function (dat, simulation = FALSE) {
  if (simulation) {
    tmp <- dat %>% dplyr::group_by_("sim", "id")
  } else {
    tmp <- dat %>% dplyr::group_by_("id")
  }
  tmp2 <- tmp %>% dplyr::arrange_("time") %>% dplyr::mutate(time = c(time[1], diff(time)))
  if (simulation) {
    return(tmp2 %>% dplyr::arrange_("sim", "id", "time"))
  } else {
    return(tmp2 %>% dplyr::arrange_("id", "time"))
  }
}

convert_from_dense_grid <- function (dat) { 
  ## Note RK: only for a single trial, requires a loop or ddply for multiple subproblems
  tmp <- dat %>% dplyr::group_by_("id")
  if("rtte" %in% names(dat)) {
    tmp <- tmp %>% dplyr::filter(rtte == 1)
  }
  tmp2 <- rbind(tmp %>% dplyr::filter(length(time) > 1) %>% dplyr::mutate(time = time - c(0,time[1:(length(time)-1)])),
                tmp %>% dplyr::filter(length(time) == 1) )
  return(tmp2 %>% dplyr::arrange_("id", "time"))
}
