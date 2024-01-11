suppressPackageStartupMessages(library('tidyverse'))

setwd(file.path('~/Dropbox/workspace/vote_projections/'))

# read in JS estimator and prediction functions
source("helper_fns.R")

# js_yhat_param = 0
js_yhat_param = "mean"

# MIT Election Data + Science lab
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX
# load('1976-2016-president.RData')
load('1976-2020-president.RData')
pres_dat <- x

# Clean up party
pres_dat$party <- tolower(pres_dat$party_simplified)
pres_dat <- pres_dat[which(pres_dat$writein==FALSE),]


# # MIT Election Data + Science lab
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IG0UN2
# load('1976-2018-house.RData')
house_dat <- read_tsv("1976-2022-house.tab")

# Clean things up
house_dat$candidatevotes <- gsub(",", "", house_dat$candidatevotes)
house_dat$candidatevotes <- as.numeric(house_dat$candidatevotes)

house_dat$party <- tolower(house_dat$party)
house_dat$party[str_detect(string = house_dat$party, pattern = "repub")] <- "republican"
house_dat$party[str_detect(string = house_dat$party, pattern = "democrat")] <- "democrat"


# Backtesting (very back of envelope): 
library(purrr)

years <- c(2004,2008,2012,2016,2020)
key_state_cutoff <- .99 # all data


mae_projection <- map_dbl(years, ~ 
                            backtest_metrics(
                              backtest(pres_dat, house_dat
                                       , house_cycles = c(.x - 2, .x - 6)
                                       , pres_cycles =  c(.x - 4, .x - 8)
                                       , pres_weight = .6
                                       # , js_yhat_param = 0
                                       , js_yhat_param = "mean"
                              )
                              , key_state_cutoff = key_state_cutoff
                            )[[1]] )

mae_projection_dumb <- map_dbl(years, ~ 
                                 backtest_metrics(
                                   backtest(pres_dat, house_dat
                                            , house_cycles = c(.x - 2, .x - 6)
                                            , pres_cycles =  c(.x - 4, .x - 8)
                                            , pres_weight = .6
                                            # , js_yhat_param = 0
                                            , js_yhat_param = "mean"
                                   )
                                   , key_state_cutoff = key_state_cutoff
                                 )[[2]] )

bt_dat <- rbind(data.frame(years, mae=mae_projection, proj_type = "proj"), 
                cbind(years, mae=mae_projection_dumb, proj_type = "prev pres"))

bt_dat$years <- as.numeric(bt_dat$years)
bt_dat$mae <- as.numeric(bt_dat$mae)

bt_dat %>% group_by(proj_type) %>% summarise(mean(mae))

ggplot(data=bt_dat, aes(x=years, y=mae, col = proj_type)) + geom_line() +
  xlab("Election year") +
  ylab("Mean Abs Error in All States") +
  scale_x_continuous(breaks=seq(2004,2020,by=4)) +
  theme_bw() 
ggsave("AllStates_MAE_projections.png", width = 5, height = 4)


key_state_cutoff <- .15

mae_projection <- map_dbl(years, ~ 
                            backtest_metrics(
                              backtest(pres_dat, house_dat
                                       , house_cycles = c(.x - 2, .x - 6)
                                       , pres_cycles =  c(.x - 4, .x - 8)
                                       , pres_weight = .6
                                       # , js_yhat_param = 0
                                       , js_yhat_param = "mean"
                              )
                              , key_state_cutoff = key_state_cutoff
                            )[[1]] )

mae_projection_dumb <- map_dbl(years, ~ 
                                 backtest_metrics(
                                   backtest(pres_dat, house_dat
                                            , house_cycles = c(.x - 2, .x - 6)
                                            , pres_cycles =  c(.x - 4, .x - 8)
                                            , pres_weight = .6
                                            # , js_yhat_param = 0
                                            , js_yhat_param = "mean"
                                   )
                                   , key_state_cutoff = key_state_cutoff
                                 )[[2]] )

bt_dat <- rbind(data.frame(years, mae=mae_projection, proj_type = "proj"), 
                cbind(years, mae=mae_projection_dumb, proj_type = "prev pres"))

bt_dat$years <- as.numeric(bt_dat$years)
bt_dat$mae <- as.numeric(bt_dat$mae)

bt_dat %>% group_by(proj_type) %>% summarise(mean(mae))

ggplot(data=bt_dat, aes(x=years, y=mae, col = proj_type)) + geom_line() +
  xlab("Election year") +
  ylab("Mean Abs Error in Battleground States (<15%)") +
  scale_x_continuous(breaks=seq(2004,2020,by=4)) +
  theme_bw() 
ggsave("Battleground_MAE_projections_proj_v_prev_pres_returns.png", width = 5, height = 4)

ggplot(data=subset(bt_dat, proj_type == "proj"), aes(x=years, y=mae)) + geom_line() +
  xlab("Election year") +
  ylab("Mean Abs Error in Battleground States (<15%)") +
  scale_x_continuous(breaks=seq(2004,2020,by=4)) +
  theme_bw() 

ggsave("Battleground_MAE_projections.png", width = 5, height = 4)



