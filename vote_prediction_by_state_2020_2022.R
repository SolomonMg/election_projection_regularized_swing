suppressPackageStartupMessages(library('tidyverse'))

setwd(file.path('~/Dropbox/workspace/vote_projections/'))

# read in JS estimator and prediction functions
source("helper_fns.R")

# Set year for main analysis (backtesting functions come later)
this_election <- 2024

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



# Get penalized slopes for pres and house returns for last two elections: 
margin_state_pres <- election_forecast(pres_dat, 
                                       latest_cycle = this_election-4, 
                                       last_cycle = this_election-8, 
                                       js_yhat_param = js_yhat_param)

margin_state_house <- election_forecast(house_dat, 
                                        latest_cycle = this_election-2, 
                                        last_cycle = this_election-6, 
                                        js_yhat_param = js_yhat_param)

state_po_margin <- combine_house_pres(margin_state_pres, 
                                      margin_state_house, 
                                      pres_weight = .6)

# Which states are predicted to be relatively close: 
key_state_pos <- state_po_margin$state_po[which(abs(state_po_margin$swing_proj_pres_house_swing_js) < .15)]

# Take a look 
print.data.frame(state_po_margin %>% 
    filter(state_po %in% key_state_pos) %>%
      mutate(proj=round(swing_proj_pres_house_swing_js,4)) %>% 
    select(state_po,
           proj
           # DR_win_margin_pct_latest_cycle_pres,
           # DR_win_margin_pct_latest_cycle_house
           # swing_proj_js_pres, 
           # swing_proj_house_margin_swing_js,
           ) %>%
      arrange(proj)
    )



# pres_weight = .6
# qplot(swing_proj_pres_house_swing_js, data = state_po_margin)
# qplot(DR_win_margin_pct_swing_js_pres, data = state_po_margin)
# qplot(DR_win_margin_pct_swing_js_house, data = state_po_margin)
# qplot(DR_win_margin_pct_swing_js_house * (1-pres_weight) + DR_win_margin_pct_swing_js_pres * (pres_weight), data = state_po_margin)

## WHAT IS HAPPENING IN WI, GA, AK?????
## presidential is very different in WI, house in GA, AK

js_yhat_param = "mean"
margin_state_pres <- election_forecast(pres_dat, 
                                       latest_cycle = this_election-4, 
                                       last_cycle = this_election-8, 
                                       js_yhat_param = js_yhat_param)

margin_state_house <- election_forecast(house_dat, 
                                        latest_cycle = this_election-2, 
                                        last_cycle = this_election-6, 
                                        js_yhat_param = js_yhat_param)

state_po_margin_mean <- combine_house_pres(margin_state_pres, 
                                      margin_state_house, 
                                      pres_weight = .6)

state_po_margin_mean$js_param <- "mean"
state_po_margin$js_param <- "0"
plotdat <- rbind(state_po_margin_mean, state_po_margin)

plotdat <- plotdat %>% filter(state_po %in% key_state_pos)

plotdat$state_po <- reorder(plotdat$state_po, plotdat$swing_proj_pres_house_swing_js)

ggplot(plotdat, aes(x = swing_proj_pres_house_swing_js, y = factor(state_po))) + 
  geom_point() + 
  facet_wrap(~js_param) +
  theme_bw() 

names(state_po_margin)
plotdat %>% filter(state_po == 'NC') %>% group_by(js_param) %>%
  summarise(presswing=DR_win_margin_pct_swing_pres, 
            houseswing=DR_win_margin_pct_swing_house,
            presswing_js=DR_win_margin_pct_swing_js_pres, 
            houseswing_js=DR_win_margin_pct_swing_js_house,
            ) 


state_po_margin %>% 
  summarise(pres=mean(DR_win_margin_pct_swing_pres, na.rm=T), 
            house=mean(DR_win_margin_pct_swing_house, na.rm=T)) 
  
# write_csv(state_po_margin, "state_po_margin_2024_proj.csv")

