# 2022 SotR presentation codes


# 01 load data, packages, and fxns ----------------------------------------

## load all necessary data
source(here::here('R', '00_loadpackages.R'))
source(here::here('R', '02.1_load_wrangle_NUT.R'))
source(here::here('R', '02.2_load_wrangle_WQ-and-MET.R'))

## load graphing fxns
source(here::here('R', '03_graph_fxns.R'))



# 02 plots ----------------------------------------------------------------

seasons <- tribble(
  ~month, ~season,
  "Jan", "Dry",
  "Feb", "Dry",
  "Mar", "Dry",
  "Apr", "Dry",
  "May", "Wet",
  "Jun", "Wet",
  "Jul", "Wet",
  "Aug", "Wet",
  "Sep", "Wet",
  "Oct", "Dry",
  "Nov", "Dry",
  "Dec", "Dry"
)

## 02.1 CHLA ----

### by site, boxplot, agm
### PI ----
boxplot_currentyear(station = "gtmpinut", 
                    param = 0, 
                    threshold = 6.6) +
  labs(title = "Pine Island") +
  annotate("text",
           x = "Mar",
           y = 16,
           size = 3,
           color = "blue",
           label = "State Threshold 6.6 (\U00B5g/L)")

agm(station = "gtmpinut", 
    param = 0, 
    threshold = 6.6) +
  labs(title = "Pine Island") +
  annotate("text",
           x = "2006",
           y = 8,
           size = 3,
           color = "blue",
           label = "State Threshold 6.6 (\U00B5g/L)")

### SS ----
boxplot_currentyear(station = "gtmssnut", 
                    param = 0, 
                    threshold = 4.0) +
  labs(title = "San Sebastian") +
  annotate("text",
           x = "Mar",
           y = 16,
           size = 3,
           color = "blue",
           label = "State Threshold 4.0 (\U00B5g/L)")

agm(station = "gtmssnut",
          param = 0,
          threshold = 4.0) +
  labs(title = "San Sebastian") +
  annotate("text",
           x = "2006",
           y = 6,
           size = 3,
           color = "blue",
           label = "State Threshold 4.0 (\U00B5g/L)")
### FM ----
boxplot_currentyear(station = "gtmfmnut", 
                    param = 0, 
                    threshold = 5.5) +
  labs(title = "Fort Matanzas") +
  annotate("text",
           x = "Mar",
           y = 24,
           size = 3,
           color = "blue",
           label = "State Threshold 5.5 (\U00B5g/L)")

agm(station = "gtmfmnut", 
    param = 0, 
    threshold = 5.5) +
  labs(title = "Fort Matanzas") +
  annotate("text",
           x = "2006",
           y = 6,
           size = 3,
           color = "blue",
           label = "State Threshold 5.5 (\U00B5g/L)")

### PC ----
boxplot_currentyear(station = "gtmpcnut", 
                    param = 0, 
                    threshold = 4.3) +
  labs(title = "Pellicer Creek") +
  annotate("text",
           x = "Mar",
           y = 38,
           size = 3,
           color = "blue",
           label = "State Threshold 4.3 (\U00B5g/L)")

agm(station = "gtmpcnut",
          param = 0,
          threshold = 4.3) +
  labs(title = "Pellicer Creek") +
  annotate("text",
           x = "2006",
           y = 16,
           size = 3,
           color = "blue",
           label = "State Threshold 4.3 (\U00B5g/L)")

# create multiplot for presentation to combine SS and PC plots (will need to save as objects)
SS + (PC + labs(y = ""))

## 02.1.1 TAL sensor CHLA ----

chla_dat <- tribble(
  ~MONITORING_PROGRAM, ~type,
  1, "Grab",
  2, "Diel"
)

pc_chla_nut <- NUT %>% 
  filter(STATION_CODE == "gtmpcnut" & DATE_TIME_STAMP > "2019-06-01 00:00") %>% 
  select(DATE_TIME_STAMP, MONITORING_PROGRAM, CHLA_N, F_CHLA_N) %>% 
  filter(!grepl("CUS|-3", F_CHLA_N)) %>% 
  left_join(chla_dat, by = "MONITORING_PROGRAM") %>% 
  select(DATE_TIME_STAMP, CHLA_N, type) %>% 
  rename(datetimestamp = DATE_TIME_STAMP,
         CHLA = CHLA_N)

pc_chla_sonde <- WQ %>% 
  filter(station == "gtmpcwq" & !is.na(chlfluor)) %>% 
  select(datetimestamp, chlfluor) %>% 
  mutate(type = "Sonde") %>% 
  rename(CHLA = chlfluor)

pc_chla <- bind_rows(pc_chla_nut, pc_chla_sonde)
rm(pc_chla_nut, pc_chla_sonde, chla_dat)

dte_formatter <- function(x) { 
  #formatter for axis labels: J12, F12, M12, etc... 
  mth <- substr(format(x, "%b"),1,1) 
  mth 
} 

#First we have to pull out the January dates from the data
pc_chla %>% 
  ggplot() +
  geom_point(data = filter(pc_chla, type == "Sonde"), 
             aes(x = datetimestamp, y = CHLA, color = "Sonde"), 
             shape = 1) +
  geom_point(data = filter(pc_chla, type == "Diel"),
             aes(x = datetimestamp, y = CHLA, color = "Diel"),
             size = 3) +
  geom_point(data = filter(pc_chla, type == "Grab"),
             aes(x = datetimestamp, y = CHLA, color = "Grab"),
             size = 4) +
  scale_colour_manual(name = '', 
                      values = c('Grab'='#0072B2', 
                                 'Diel'='#E69F00', 
                                 'Sonde'='grey70')) +
  geom_vline(xintercept = as.numeric(pc_chla$datetimestamp[yday(pc_chla$datetimestamp) == 1]), 
             colour = "grey60") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = "1 months", labels = dte_formatter, expand = c(0,0)) +
  theme_classic() +
  theme(axis.ticks.x = element_line(color = "black"),
        axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 18)) +
  labs(x = "",
       y = "Chlorophyll a (\U00B5g/L)") +
  annotate("text",
           x = as.POSIXct("2019-07-15 00:00"),
           y = 75,
           size = 5,
           color = "black",
           label = "2019") +
  annotate("text",
           x = as.POSIXct("2020-02-15 00:00"),
           y = 75,
           size = 5,
           color = "black",
           label = "2020") +
  annotate("text",
           x = as.POSIXct("2021-02-15 00:00"),
           y = 75,
           size = 5,
           color = "black",
           label = "2021")

rm(pc_chla, dte_formatter)



## 02.2 TN ----
### PI ----
boxplot_currentyear(station = "gtmpinut",
                    param = 1,
                    threshold = 0.65) +
  labs(title = "Pine Island") +
  annotate("text",
           x = "Mar",
           y = 1.2,
           size = 3,
           color = "blue",
           label = "State Threshold 0.65 (mg/L)")

agm(station = "gtmpinut", 
    param = 1, 
    threshold = 0.65) +
  labs(title = "Pine Island") +
  annotate("text",
           x = "2006",
           y = 0.68,
           size = 3,
           color = "blue",
           label = "State Threshold 0.65 (\U00B5g/L)")

### SS ----
boxplot_currentyear(station = "gtmssnut", 
                    param = 1, 
                    threshold = 0.55) +
  labs(title = "San Sebastian") +
  annotate("text",
           x = "Mar",
           y = 1.0,
           size = 3,
           color = "blue",
           label = "State Threshold 0.55 (mg/L)")

agm(station = "gtmssnut", 
    param = 1, 
    threshold = 0.55) +
  labs(title = "San Sebastian") +
  annotate("text",
           x = "2006",
           y = 0.58,
           size = 3,
           color = "blue",
           label = "State Threshold 0.55 (\U00B5g/L)")

### FM ----
boxplot_currentyear(station = "gtmfmnut", 
                    param = 1, 
                    threshold = 0.55) +
  labs(title = "Fort Matanzas") +
  annotate("text",
           x = "Mar",
           y = 1.0,
           size = 3,
           color = "blue",
           label = "State Threshold 0.55 (mg/L)")

agm(station = "gtmfmnut", 
    param = 1, 
    threshold = 0.53) +
  labs(title = "Fort Matanzas") +
  annotate("text",
           x = "2006",
           y = 0.55,
           size = 3,
           color = "blue",
           label = "State Threshold 0.53 (\U00B5g/L)")

### PC ----
boxplot_currentyear(station = "gtmpcnut", 
                    param = 1, 
                    threshold = 1.10) +
  labs(title = "Pellicer Creek") +
  annotate("text",
           x = "Mar",
           y = 1.7,
           size = 3,
           color = "blue",
           label = "State Threshold 1.10 (mg/L)")

agm(station = "gtmpcnut", 
    param = 1, 
    threshold = 1.10) +
  labs(title = "Pellicer Creek") +
  annotate("text",
           x = "2006",
           y = 1.15,
           size = 3,
           color = "blue",
           label = "State Threshold 1.1 (\U00B5g/L)")
## 02.2.1 Chemical Forms of Nitrogen ----

# !! this uses dfs from the 'N_P_forms_statistics.R' script

# stacked barchart
sites %>%
  select(STATION_CODE, DATE, DON, DIN, PN) %>%
  mutate(STATION_CODE = factor(STATION_CODE,
                               levels = c("gtmpinut",
                                          "gtmssnut",
                                          "gtmfmnut",
                                          "gtmpcnut"),
                               labels = c("Pine Island",
                                          "San Sebastian",
                                          "Fort Matanzas",
                                          "Pellicer Creek"))) %>% 
  pivot_longer(cols = 3:5,
               names_to = "nitro_source",
               values_to = "conc") %>% 
  ggplot(aes(x = DATE, y = conc, fill = nitro_source, group = STATION_CODE)) +
  geom_col() +
  facet_wrap(~STATION_CODE) +
  scale_fill_okabeito(name = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = '',
       y = "Nitrogen (mg/L)")

sites %>%
  filter(STATION_CODE == "gtmpcnut") %>%
  select(DATE, DON, DIN, PN) %>% 
  pivot_longer(cols = 2:4,
               names_to = "nitro_source",
               values_to = "conc") %>% 
  ggplot(aes(x = DATE, y = conc, fill = nitro_source)) +
  geom_col(stat = "identity") +
  scale_fill_okabeito(name = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = '',
       y = "Nitrogen (mg/L)",
       title = "Pellicer Creek SWMP Station") +
  theme(axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20),
        title = element_text(size = 20),
        legend.text = element_text(size = 18))

# pie chart of chemical forms of nitrogen
# need to get Nresid from the lms of each site, it is the intercept value
# these numbers change with new data.
resid <- tribble(
  ~STATION_CODE, ~N_resid,
  "gtmpinut", 3.26439,
  "gtmssnut", 1.52725,
  "gtmfmnut", 0.9615,
  "gtmpcnut", 4.05746
)

all %>% 
  select(STATION_CODE, 
         DINuM_mean, 
         DONuM_mean,
         N_phyto_mean,
         N_sed_mean) %>% 
  left_join(resid, by = "STATION_CODE") %>% 
  rename(station = STATION_CODE,
         DIN = DINuM_mean,
         DON = DONuM_mean,
         N_phyto = N_phyto_mean,
         N_sed = N_sed_mean) %>% 
  mutate(station = factor(station, 
                          levels = c("gtmpinut",
                                     "gtmssnut",
                                     "gtmfmnut",
                                     "gtmpcnut"),
                          labels = c("PI", "SS", "FM", "PC"))) %>% 
  tidyr::pivot_longer(cols = c(2:6),
                      names_to = "Nitrogen_Source",
                      values_to = "Amt") %>% 
  ggplot() +
  geom_bar(aes(x = '', y = Amt, fill = Nitrogen_Source),
           # color = "white", 
           width = 1, 
           stat = "identity",
           position = "fill") + # by setting position = "fill" this will fill up any gaps caused by using avgs
  coord_polar("y", start = 0) +
  facet_wrap(~station, ncol = 2) +
  scale_fill_okabeito(name = '') +
  theme_void() +
  theme(strip.text = element_text(size = 20),
        legend.text = element_text(size = 18))

rm(resid)


## 02.3 TP ----

### PI ----
boxplot_currentyear(station = "gtmpinut", 
                    param = 2, 
                    threshold = 0.105) +
  labs(title = "Pine Island") +
  annotate("text",
           x = "Mar",
           y = 0.15,
           size = 3,
           color = "blue",
           label = "State Threshold 0.105 (mg/L)")

agm(station = "gtmpinut", 
    param = 2, 
    threshold = 0.105) +
  labs(title = "Pine Island") +
  annotate("text",
           x = "2006",
           y = 0.15,
           size = 3,
           color = "blue",
           label = "State Threshold 0.105 (\U00B5g/L)")

### SS ----
boxplot_currentyear(station = "gtmssnut", 
                    param = 2, 
                    threshold = 0.11) +
  labs(title = "San Sebastian") +
  annotate("text",
           x = "Mar",
           y = 0.12,
           size = 3,
           color = "blue",
           label = "State Threshold 0.11 (mg/L)")

agm(station = "gtmssnut", 
    param = 2, 
    threshold = 0.11) +
  labs(title = "San Sebastian") +
  annotate("text",
           x = "2006",
           y = 0.12,
           size = 3,
           color = "blue",
           label = "State Threshold 0.11 (\U00B5g/L)")
### FM ----
boxplot_currentyear(station = "gtmfmnut", 
                    param = 2, 
                    threshold = 0.111) +
  labs(title = "Fort Matanzas") +
  annotate("text",
           x = "Mar",
           y = 0.12,
           size = 3,
           color = "blue",
           label = "State Threshold 0.111 (mg/L)")

agm(station = "gtmfmnut", 
    param = 2, 
    threshold = 0.111) +
  labs(title = "Fort Matanzas") +
  annotate("text",
           x = "2006",
           y = 0.12,
           size = 3,
           color = "blue",
           label = "State Threshold 0.111 (\U00B5g/L)")

### PC ----
boxplot_currentyear(station = "gtmpcnut", 
                    param = 2, 
                    threshold = 0.123) +
  labs(title = "Pellicer Creek") +
  annotate("text",
           x = "Mar",
           y = 0.25,
           size = 3,
           color = "blue",
           label = "State Threshold 0.123 (mg/L)")

agm(station = "gtmpcnut", 
    param = 2, 
    threshold = 0.123) +
  labs(title = "Pellicer Creek") +
  annotate("text",
           x = "2006",
           y = 0.25,
           size = 3,
           color = "blue",
           label = "State Threshold 0.123 (\U00B5g/L)")

## 02.4 Rainfall ----

### 02.4.1 seasonal barplot ----
MET %>% 
  mutate(totprcp = totprcp * 0.0393701) %>% 
  SWMPrExtension::seasonal_barplot(param = 'totprcp',
                                   season_grps = list(c(12,1,2), c(3,4,5), c(6,7,8), c(9, 10, 11)),
                                   season_names = c('Winter', 'Spring', 'Summer', 'Fall'),
                                   hist_avg = TRUE,
                                   converted = TRUE) + 
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 0.6),
        axis.text = element_text(color = "black", size = 18),
        axis.title.y = element_text(size = 20))

### 02.4.2 yearly barplot ----
### uncomment chunk of code just to get the monthly totals in console
MET %>% 
  filter(datetimestamp > '2021-01-01 00:15') %>%
  mutate(date = as.Date(datetimestamp),
         month = lubridate::month(date, label = T),
         totprcp_in = totprcp * 0.0393701) %>% 
  # group_by(month) %>% 
  # summarise(sum = sum(totprcp_in, na.rm = T)) %>% 
  # summarise(mean = mean(sum))
  ggplot() +
  geom_col(aes(x = month, y = totprcp_in), fill = "#0075AC") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20)) +
  labs(x = '',
       y = 'Total Precipitation (in)')

## 02.5 DO ----
WQ %>% 
  filter(do_mgl < 20 & do_mgl > 0) %>% 
  mutate(station = factor(station, levels = c("gtmpiwq",
                                              "gtmsswq",
                                              "gtmfmwq",
                                              "gtmpcwq"),
                          labels = c("PI", "SS", "FM", "PC"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = station, y = do_mgl, fill = station)) + 
  scale_fill_manual(values = c("PI" = "#0072B2",
                               "SS" = "#CC7987",
                               "FM" = "#009E73",
                               "PC" = "#E69F00")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20)) +
  labs(x = "",
       y = "Dissolved Oxygen (mg/L)")

seasonal_boxplot(pi, param = 'do_mgl') + labs(title = "Pine Island") + theme(legend.position = "none")
seasonal_boxplot(ss, param = 'do_mgl') + labs(title = "San Sebastian") + theme(legend.position = "none")
seasonal_boxplot(filter(fm, do_mgl > 0) , param = 'do_mgl') + labs(title = "Fort Matanzas") + theme(legend.position = "none") # fm has some negative do values in 2003 that were not removed during qaqc
seasonal_boxplot(pc, param = 'do_mgl') + labs(title = "Pellicer Creek") + theme(legend.position = "none")


### PI ----
threshold_criteria_plot(pi, param = 'do_mgl', rng = c(2016, 2021), 
                        thresholds = c(2,5),
                        threshold_labs = c('Poor', 'Fair', 'Good'), 
                        monthly_smooth = TRUE, 
                        threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9')) +
  theme(axis.text = element_text(color = "black", size = 18),
        axis.title.y = element_text(size = 20))

### SS ----
threshold_criteria_plot(ss, param = 'do_mgl', rng = c(2016, 2021), 
                        thresholds = c(2,5),
                        threshold_labs = c('Poor', 'Fair', 'Good'), 
                        monthly_smooth = TRUE, 
                        threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9')) +
  theme(axis.text = element_text(color = "black", size = 18),
        axis.title.y = element_text(size = 20))

### FM ----
threshold_criteria_plot(fm, param = 'do_mgl', rng = c(2016, 2021), 
                        thresholds = c(2,5),
                        threshold_labs = c('Poor', 'Fair', 'Good'), 
                        monthly_smooth = TRUE, 
                        threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9')) +
  theme(axis.text = element_text(color = "black", size = 18),
        axis.title.y = element_text(size = 20))


### PC ----
threshold_criteria_plot(pc, param = 'do_mgl', rng = c(2016, 2021), 
                        thresholds = c(2,5),
                        threshold_labs = c('Poor', 'Fair', 'Good'), 
                        monthly_smooth = TRUE, 
                        threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9')) +
  theme(axis.text = element_text(color = "black", size = 18),
        axis.title.y = element_text(size = 20))



## 02.6 Salinity ----
WQ %>% 
  mutate(station = factor(station, levels = c("gtmpiwq",
                                              "gtmsswq",
                                              "gtmfmwq",
                                              "gtmpcwq"),
                          labels = c("PI", "SS", "FM", "PC"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = station, y = sal, fill = station)) + 
  scale_fill_manual(values = c("PI" = "#0072B2",
                               "SS" = "#CC7987",
                               "FM" = "#009E73",
                               "PC" = "#E69F00")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20)) +
  labs(x = "",
       y = "Salinity (psu)")

 

WQ %>% 
  mutate(month = month(datetimestamp, label = T),
         station = factor(station, levels = c("gtmpiwq",
                                              "gtmsswq",
                                              "gtmfmwq",
                                              "gtmpcwq"),
                          labels = c("PI", "SS", "FM", "PC"))) %>% 
  right_join(seasons, by = "month") %>% 
  ggplot() +
  geom_boxplot(aes(x = station, y = sal, fill = station)) + 
  scale_fill_manual(values = c("PI" = "#0072B2",
                               "SS" = "#CC7987",
                               "FM" = "#009E73",
                               "PC" = "#E69F00")) +
  facet_grid(~season) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20),
        strip.text = element_text(size = 20)) +
  labs(x = "",
       y = "Salinity (psu)")

## 02.7 fDOM ----
WQ %>% 
  mutate(station = factor(station, levels = c("gtmpiwq",
                                              "gtmsswq",
                                              "gtmfmwq",
                                              "gtmpcwq"),
                          labels = c("PI", "SS", "FM", "PC"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = station, y = fdom, fill = station)) + 
  scale_fill_manual(values = c("PI" = "#0072B2",
                               "SS" = "#CC7987",
                               "FM" = "#009E73",
                               "PC" = "#E69F00")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20)) +
  labs(x = "",
       y = "Salinity (psu)")

# N:P calculations and plots -----------------------------------------------------------
# comes from 'N_P_forms_statistics.R' script. Modifications necessary prior to running code

# redfield ratios on all data
NUT_monthly %>% 
  select(STATION_CODE, DATE, TN_avg, TP_avg) %>%
  # mutate(year = lubridate::year(DATE)) %>% 
  # filter(year < 2013) %>% 
  mutate(STATION_CODE = factor(STATION_CODE,
                               levels = c("gtmpinut",
                                          "gtmssnut",
                                          "gtmfmnut",
                                          "gtmpcnut"),
                               labels = c("PI",
                                          "SS",
                                          "FM",
                                          "PC"))) %>%
  ggplot() +
  geom_point(aes(x = TP_avg, y = TN_avg, color = STATION_CODE), color = "white", size = 3) +
  geom_abline(slope = 7.23, size = 0.5) +
  scale_color_manual(name = "Site",
                     values = c("PI" = "#0072B2",
                                "SS" = "#CC7987",
                                "FM" = "#009E73",
                                "PC" = "#E69F00")) +
  theme_classic() +
  theme(axis.text = element_text(color = "white", size= 18),
        axis.title.y = element_text(color = "black", size = 20),
        axis.title.x = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 18)) +
  labs(x = 'Total Phosphorus (mg/L)',
       y = "Total Nitrogen (mg/L)")

NUT_monthly %>% 
  select(STATION_CODE, DATE, TN_avg, TP_avg) %>%
  mutate(TN_TP = TN_avg/TP_avg) %>% 
  group_by(STATION_CODE) %>% 
  summarize(mean = mean(TN_TP, na.rm = T))


# # redfield ratios
# sites %>% 
#   select(STATION_CODE, DATE, TN_TP, DIN_DIP) %>% 
#   mutate(STATION_CODE = factor(STATION_CODE,
#                                levels = c("gtmpinut",
#                                           "gtmssnut",
#                                           "gtmfmnut",
#                                           "gtmpcnut"),
#                                labels = c("PI",
#                                           "SS",
#                                           "FM",
#                                           "PC"))) %>% 
#   ggplot() +
#   geom_boxplot(aes(x = STATION_CODE, y = TN_TP, fill = STATION_CODE)) +
#   geom_hline(yintercept = 7.2) + 
#   scale_fill_manual(values = c("PI" = "#0072B2",
#                                "SS" = "#CC7987",
#                                "FM" = "#009E73",
#                                "PC" = "#E69F00")) +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.text = element_text(color = "black", size= 18),
#         axis.title.y = element_text(color = "black", size = 20)) +
#   labs(x = '',
#        y = "TN:TP")
# 
# sites %>% 
#   select(STATION_CODE, DATE, TN_TP, DIN_DIP) %>% 
#   mutate(STATION_CODE = factor(STATION_CODE,
#                                levels = c("gtmpinut",
#                                           "gtmssnut",
#                                           "gtmfmnut",
#                                           "gtmpcnut"),
#                                labels = c("PI",
#                                           "SS",
#                                           "FM",
#                                           "PC"))) %>% 
#   ggplot() +
#   geom_point(aes(x = DATE, y = TN_TP, color = STATION_CODE), size = 3) +
#   geom_hline(yintercept = 7.2) + 
#   scale_color_manual(name = "Site", 
#                      values = c("PI" = "#0072B2",
#                                 "SS" = "#CC7987",
#                                 "FM" = "#009E73",
#                                 "PC" = "#E69F00")) +
#   theme_classic() +
#   theme(axis.text = element_text(color = "black", size= 18),
#         axis.title.y = element_text(color = "black", size = 20),
#         legend.title = element_text(color = "black", size = 18),
#         legend.text = element_text(color = "black", size = 18)) +
#   labs(x = '',
#        y = "TN:TP")

sites %>%
  select(STATION_CODE, DATE, TN, TP) %>%
  mutate(STATION_CODE = factor(STATION_CODE,
                               levels = c("gtmpinut",
                                          "gtmssnut",
                                          "gtmfmnut",
                                          "gtmpcnut"),
                               labels = c("PI",
                                          "SS",
                                          "FM",
                                          "PC"))) %>%
  ggplot() +
  geom_point(aes(x = TP, y = TN, color = STATION_CODE), size = 3) +
  geom_abline(slope = 7.23, size = 0.5) +
  scale_color_manual(name = "Site",
                     values = c("PI" = "#0072B2",
                                "SS" = "#CC7987",
                                "FM" = "#009E73",
                                "PC" = "#E69F00")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20),
        axis.title.x = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 18)) +
  labs(x = 'Total Phosphorus (mg/L)',
       y = "Total Nitrogen (mg/L)")

