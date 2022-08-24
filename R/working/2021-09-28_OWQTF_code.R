# owqtf plots
# first load packages
source(here::here('R', '00_loadpackages.R'))
# load data
load(here::here('output', 'data', 'NUT.RData'))
# check it
# glimpse(NUT)

# 01 select parameters of interest ----

TN_f <- NUT %>% 
  dplyr::select(1:4, TN, F_TN) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_TN) | is.na(F_TN)) %>%
  dplyr::select(-F_TN) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(STATION_CODE, DATE, REP, TN) 

TP_f <- NUT %>% 
  dplyr::select(1:4, TP, F_TP) %>%
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_TP) | is.na(F_TP)) %>% 
  dplyr::select(-F_TP) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP)     

CHLA_f <- NUT %>% 
  dplyr::select(1:4, CHLA_N, F_CHLA_N) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_CHLA_N) | is.na(F_CHLA_N)) %>% 
  dplyr::select(-F_CHLA_N) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP) 

ENTERO_f <- NUT %>% 
  dplyr::select(1:4, ENTERO_MPN, F_ENTERO_MPN) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_ENTERO_MPN) | is.na(F_ENTERO_MPN)) %>% 
  dplyr::select(-F_ENTERO_MPN) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP) 

FECAL_f <- NUT %>% 
  dplyr::select(1:4, FECCOL_CFU, F_FECCOL_CFU) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_FECCOL_CFU) | is.na(F_FECCOL_CFU)) %>% 
  dplyr::select(-F_FECCOL_CFU) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP) 


# 02 merge all parameters into one new df --------------------------------

NUT_f <- dplyr::left_join(TN_f, TP_f, by = c("STATION_CODE", "DATE", "REP")) %>% 
  dplyr::left_join(CHLA_f, by = c("STATION_CODE", "DATE", "REP")) %>% 
  dplyr::left_join(ENTERO_f, by = c("STATION_CODE", "DATE", "REP")) %>% 
  dplyr::left_join(FECAL_f, by = c("STATION_CODE", "DATE", "REP"))

# clean environment
rm(CHLA_f, TN_f, TP_f, ENTERO_f, FECAL_f)


# 03 calculations --------------------------------------------------------------

## 03.1 monthly average ----
NUT_monthly <- NUT_f %>% 
          dplyr::group_by(STATION_CODE, DATE) %>% 
          dplyr::summarise(TN_avg = mean(TN, na.rm = TRUE),
                           TP_avg = mean(TP, na.rm = TRUE),
                           CHLA_avg = mean(CHLA_N, na.rm = TRUE),
                           ENTERO_avg = mean(ENTERO_MPN, na.rm = TRUE),
                           FECAL_avg = mean(FECCOL_CFU, na.rm = TRUE),
                           .groups = "keep") %>% 
          dplyr::mutate(YEAR = lubridate::year(DATE), 
                        MONTH_abb = lubridate::month(DATE, label = TRUE, abbr = TRUE),
                        MONTH = lubridate::month(DATE),
                        STATION_CODE = factor(STATION_CODE,
                                              levels = c("gtmpinut",
                                                         "gtmssnut",
                                                         "gtmfmnut",
                                                         "gtmpcnut")))

## 03.2 monthly averages to yearly ----
# annual geometric mean function
gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
# or use the psych::geometric.mean() function

NUT_yearly <- NUT_monthly %>% 
  dplyr::group_by(STATION_CODE, YEAR) %>% 
  dplyr::summarise(TN_agm = psych::geometric.mean(TN_avg, na.rm = T),
                   TP_agm = psych::geometric.mean(TP_avg, na.rm = T),
                   CHLA_agm = psych::geometric.mean(CHLA_avg, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(YEAR = forcats::as_factor(YEAR))

# 04 plotting functions ----

## 04.1 monthly-boxplot-w-current-YR ----

boxplot_currentyear <- function(station, param, threshold) {
  # parameters: station, param, threshold
  # 0 is CHLA_avg, 1 is TN_avg, 2 is TP_avg

if (param == 0) {  
  boxplot <- ggplot(data = NUT_monthly, 
                    aes(x = MONTH_abb, y = CHLA_avg)) +
                geom_boxplot(data = filter(NUT_monthly, STATION_CODE == station & YEAR < 2021), 
                             aes(fill = "2002-2020")) +
                geom_point(data = filter(NUT_monthly, STATION_CODE == station & YEAR == 2021), 
                           aes(color = "2021"),
                           size = 4) +
                geom_hline(yintercept = threshold, color = "blue",
                           linetype = "dashed") +
                scale_color_manual(name = "", 
                                   values = c("2021" = "red")) +
                scale_fill_manual(name = "",
                                  values = c("2002-2020" = "white")) +
                theme_classic() +
                theme(legend.position = "bottom",
                      axis.text = element_text(color = "black")) +
    labs(x = "",
         y = "Mean Monthly Chlorophyll-a (\U00B5g/L)")
} else if (param == 1) {
  
  boxplot <- ggplot(data = NUT_monthly, 
                    aes(x = MONTH_abb, y = TN_avg)) +
    geom_boxplot(data = filter(NUT_monthly, STATION_CODE == station & YEAR < 2021), 
                 aes(fill = "2002-2020")) +
    geom_point(data = filter(NUT_monthly, STATION_CODE == station & YEAR == 2021), 
               aes(color = "2021"),
               size = 4) +
    geom_hline(yintercept = threshold, color = "blue",
               linetype = "dashed") +
    scale_color_manual(name = "", 
                       values = c("2021" = "red")) +
    scale_fill_manual(name = "",
                      values = c("2002-2020" = "white")) +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text = element_text(color = "black")) +
    labs(x = "",
         y = "Mean Monthly Total Nitrogen (mg/L)")
  
} else {
  boxplot <- ggplot(data = NUT_monthly, 
                    aes(x = MONTH_abb, y = TP_avg)) +
    geom_boxplot(data = filter(NUT_monthly, STATION_CODE == station & YEAR < 2021), 
                 aes(fill = "2002-2020")) +
    geom_point(data = filter(NUT_monthly, STATION_CODE == station & YEAR == 2021), 
               aes(color = "2021"),
               size = 4) +
    geom_hline(yintercept = threshold, color = "blue",
               linetype = "dashed") +
    scale_color_manual(name = "", 
                       values = c("2021" = "red")) +
    scale_fill_manual(name = "",
                      values = c("2002-2020" = "white")) +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text = element_text(color = "black")) +
    labs(x = "",
         y = "Mean Monthly Total Phosphorus (mg/L)")
}
}

# pc_chla_boxplot <- boxplot_currentyear(station = "gtmpcnut", param = 0, threshold = 4.3)
# plotly::ggplotly(pc_chla_boxplot + labs(x = "",
#                        y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
#                        title = "Pellicer Creek") +
#   annotate("text",
#            x = "Mar",
#            y = 38,
#            size = 3,
#            color = "blue",
#            label = "State Threshold 4.3 (\U00B5g/L)")
# )
# 
# boxplot_currentyear(station = "gtmpcnut", param = 0, threshold = 4.3) +
#   labs(x = "",
#        y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
#        title = "Pellicer Creek") +
#   annotate("text",
#            x = "Mar",
#            y = 38,
#            size = 3,
#            color = "blue",
#            label = "State Threshold 4.3 (\U00B5g/L)")

## 04.2 yearly agms ----

agm <- function(station, param, threshold) {
  if (param == 0) {
    agm <- NUT_yearly  %>% 
            dplyr::filter(STATION_CODE == station) %>% 
            ggplot(aes(x = YEAR, y = CHLA_agm, group = 1)) +
            geom_line() +
            geom_point(aes(color = CHLA_agm > threshold), size = 3) +
            geom_hline(yintercept = threshold, color = "blue",
                       linetype = "dashed") +
            scale_color_manual(name = "", values = c("black", "red")) +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text = element_text(color = "black"),
                  axis.text.x = element_text(angle = 45, 
                                             vjust = 0.6)) +
            labs(x = "",
                 y = "Geo.Mean Annual Chlorophyll-a (\U00B5g/L)")
  } else if (param == 1) {
    agm <- NUT_yearly  %>% 
            dplyr::filter(STATION_CODE == station) %>% 
            ggplot(aes(x = YEAR, y = TN_agm, group = 1)) +
            geom_line() +
            geom_point(aes(color = TN_agm > threshold), size = 3) +
            geom_hline(yintercept = threshold, color = "blue",
                       linetype = "dashed") +
            scale_color_manual(name = "", values = c("black", "red")) +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text = element_text(color = "black"),
                  axis.text.x = element_text(angle = 45, 
                                             vjust = 0.6)) +
            labs(x = "",
                 y = "Geo.Mean Annual Total Nitrogen (mg/L)")
  } else {
    agm <- NUT_yearly  %>% 
            dplyr::filter(STATION_CODE == station) %>% 
            ggplot(aes(x = YEAR, y = TP_agm, group = 1)) +
            geom_line() +
            geom_point(aes(color = TP_agm > threshold), size = 3) +
            geom_hline(yintercept = threshold, color = "blue",
                       linetype = "dashed") +
            scale_color_manual(name = "", values = c("black", "red")) +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text = element_text(color = "black"),
                  axis.text.x = element_text(angle = 45, 
                                             vjust = 0.6)) +
            labs(x = "",
                 y = "Geo.Mean Annual Total Phosphorus (mg/L)")
  }
}

# a <- agm(station = "gtmpcnut",
#     param = 0,
#     intercept = 4.3)
# a

## 04.3 Static Boxplots ----

# CHLA
## PI
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
## SS
SS <- agm(station = "gtmssnut", 
          param = 0, 
          threshold = 4.0) +
  labs(title = "San Sebastian") +
  annotate("text",
           x = "2006",
           y = 6,
           size = 3,
           color = "blue",
           label = "State Threshold 4.0 (\U00B5g/L)")
## FM
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
## PC
PC <- agm(station = "gtmpcnut", 
          param = 0, 
          threshold = 4.3) +
  labs(title = "Pellicer Creek") +
  annotate("text",
           x = "2006",
           y = 16,
           size = 3,
           color = "blue",
           label = "State Threshold 4.3 (\U00B5g/L)")

SS + (PC + labs(y = ""))
