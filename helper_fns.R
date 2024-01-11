suppressPackageStartupMessages(library('tidyverse'))

# James-Stein estimator
js_est <- function(y, 
                   y_hat, 
                   c_scale = 1, narm=F){
  if(y_hat=="mean"){
    y_hat <- mean(y, na.rm=narm)
  }
  n <- length(na.omit(y))
  l2y <- sum( (y-y_hat)^2, na.rm=narm)
  sigma2 <- var(y, na.rm=narm)
  c_factor <- (1 - ((n - 3) * sigma2) / l2y)
  est <- y_hat + c_scale * c_factor * (y-y_hat)
  return(est)
}


election_forecast <- function(dat, latest_cycle, last_cycle, 
                              js_yhat_param = 0){
  cat(c(last_cycle, latest_cycle))
  dat <- dat %>% filter(party %in% c('republican', 'democrat'), year %in% c(last_cycle, latest_cycle)) %>%
    select(state_po, party, year, writein, candidatevotes, totalvotes)
  
  dat$candidatevotes <- as.numeric(dat$candidatevotes)
  dat$totalvotes <- as.numeric(dat$totalvotes)
  dat$cycle <- factor(dat$year) 
  levels(dat$cycle) <- c("last_cycle", "latest_cycle")
  
  
  # Sum over write-in votes
  dat_sum <- dat %>% group_by(party, state_po, cycle) %>%
    summarise(candidatevotes = sum(candidatevotes), 
              totalvotes = sum(totalvotes))
  
  
  # pivot to wider data
  wide_dat <- dat_sum %>% 
    pivot_wider(names_from = c(party, cycle),
                values_from = c(candidatevotes, totalvotes))
  
  names(dat)
  names(wide_dat)
  margin_state <- wide_dat %>% 
    group_by(state_po) %>% 
    summarise(
      total_votes_last_cycle = 
        sum(totalvotes_democrat_last_cycle, totalvotes_republican_last_cycle)/2,
      DR_win_margin_last_cycle = 
        candidatevotes_democrat_last_cycle - 
        candidatevotes_republican_last_cycle,
      DR_win_margin_pct_latest_cycle = 
        (candidatevotes_democrat_latest_cycle - 
           candidatevotes_republican_latest_cycle)/
        sum(candidatevotes_democrat_latest_cycle,
            candidatevotes_republican_latest_cycle),
      DR_win_margin_pct_last_cycle = 
        (candidatevotes_democrat_last_cycle - 
           candidatevotes_republican_last_cycle)/
        sum(candidatevotes_democrat_last_cycle,
            candidatevotes_republican_last_cycle)
    )
  
  # Estimate swing
  margin_state <- margin_state %>% mutate(
    # DR_win_margin_pct_swing_no_js = DR_win_margin_pct_latest_cycle-DR_win_margin_pct_last_cycle,
    DR_win_margin_pct_swing = DR_win_margin_pct_latest_cycle-DR_win_margin_pct_last_cycle
  )
  
  
  # Shrink swing
  margin_state <- margin_state %>% mutate(
    DR_win_margin_pct_swing_js = js_est(DR_win_margin_pct_swing, 
                                        y_hat = js_yhat_param, 
                                        c_scale = 1, narm=T)
  )
  
  
  # Add swing to last_cycle 
  margin_state <- margin_state %>% mutate(
    swing_proj = DR_win_margin_pct_latest_cycle + DR_win_margin_pct_swing,
    swing_proj_js = DR_win_margin_pct_latest_cycle + DR_win_margin_pct_swing_js
  )
  
  return(margin_state)
}




combine_house_pres <- function(margin_state_pres, 
                               margin_state_house, 
                               pres_weight = .6){
  state_po_margin <- left_join(margin_state_pres, margin_state_house, 
                               by = "state_po", suffix = c("_pres", "_house"))
  state_po_margin <- state_po_margin %>% mutate(
    swing_proj_pres_house_swing_js = 
      DR_win_margin_pct_latest_cycle_pres + DR_win_margin_pct_swing_js_pres * pres_weight + 
      DR_win_margin_pct_swing_js_house * (1-pres_weight),
    swing_proj_pres_house_margin_swing_js = 
      swing_proj_js_pres * pres_weight + 
      swing_proj_js_house * (1-pres_weight)
  )
  
  return(state_po_margin)
}




backtest <- function(pres_dat, 
                     house_dat, 
                     house_cycles, 
                     pres_cycles,
                     pres_weight = .8,
                     js_yhat_param = 0){
  margin_state_pres_prev <- election_forecast(pres_dat, 
                                              latest_cycle = pres_cycles[1], 
                                              last_cycle = pres_cycles[2], 
                                              js_yhat_param = js_yhat_param)
  
  margin_state_house_prev <- election_forecast(house_dat, 
                                               latest_cycle = house_cycles[1], 
                                               last_cycle = house_cycles[2], 
                                               js_yhat_param = js_yhat_param)
  
  state_po_margin_prev <- combine_house_pres(margin_state_pres_prev, 
                                             margin_state_house_prev, 
                                             pres_weight = pres_weight)
  state_po_margin <- election_forecast(pres_dat, 
                                       latest_cycle = pres_cycles[1] + 4, 
                                       last_cycle = pres_cycles[2] + 4, 
                                       js_yhat_param = js_yhat_param)
  state_po_margin$prev_swing_proj_js_pres <- state_po_margin_prev$swing_proj_js_pres[match(state_po_margin$state_po, state_po_margin_prev$state_po)]
  state_po_margin$prev_swing_proj_pres_house_swing_js <- state_po_margin_prev$swing_proj_pres_house_swing_js[match(state_po_margin$state_po, state_po_margin_prev$state_po)]
  # state_po_margin$prev_swing_proj_pres_house_margin_swing_js <- state_po_margin_prev$swing_proj_pres_house_margin_swing_js[match(state_po_margin$state_po, state_po_margin_prev$state_po)]
  return(state_po_margin)
}



backtest_metrics <- function(state_po_margin,
                             key_state_cutoff = .15){
  
  state_po_margin <- state_po_margin %>% mutate(
    abs_err = 
      abs(DR_win_margin_pct_latest_cycle - prev_swing_proj_pres_house_swing_js),
    abs_err_dumb =
      abs(DR_win_margin_pct_latest_cycle - DR_win_margin_pct_last_cycle),
    sqerr = (DR_win_margin_pct_latest_cycle - prev_swing_proj_pres_house_swing_js)^2
  )
  
  # Which states are predicted to be relatively close: 
  if (is.numeric(key_state_cutoff)) key_state_pos <- state_po_margin$state_po[which(abs(state_po_margin$swing_proj_js) < key_state_cutoff)]
  
  if (is.character(key_state_cutoff)) key_state_pos <- key_state_cutoff
  
  list(
    sum(state_po_margin$abs_err[which(state_po_margin$state_po %in% key_state_pos)], na.rm=T),
    sum(state_po_margin$abs_err_dumb[which(state_po_margin$state_po %in% key_state_pos)], na.rm=T),
    sum(state_po_margin$sqerr[which(state_po_margin$state_po %in% key_state_pos)], na.rm=T)
  )
}