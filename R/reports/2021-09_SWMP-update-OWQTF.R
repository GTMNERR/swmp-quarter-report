# water quality dissolved oxygen
# first load packages
# source('R/00_loadpackages.R')

# load wq data
# if haven't run in a while, you may want update the data and the subsequent .RData object below
load(here::here('output', 'data', 'WQ.RData'))

# create threshold plots of DO

seasonal_barplot(MET, param = 'totprcp',
                 season_grps = list(c(12,1,2), c(3,4,5), c(6,7,8), c(9, 10, 11)),
                 season_names = c('Winter', 'Spring', 'Summer', 'Fall'),
                 hist_avg = TRUE) + theme(axis.text.x = element_text(angle = 45,
                                                                     vjust = 0.6,
                                                                     color = "black"))
wq <- WQ %>% filter(station == "gtmsswq") %>% select(-station)

threshold_criteria_plot(ss, param = 'do_pct', rng = 2020)
