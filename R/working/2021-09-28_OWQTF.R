# owqtf plots
# first load packages
# source(here::here('R', '00_loadpackages.R'))
# load data
# load(here::here('output', 'data', 'NUT.RData'))
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


# 03 monthly --------------------------------------------------------------

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

hist_minmax <- NUT_monthly %>% 
  dplyr::group_by(STATION_CODE, MONTH) %>% 
  dplyr::summarise(TN_min = min(TN_avg, na.rm = TRUE),
                   TN_max = max(TN_avg, na.rm = TRUE),
                   TP_min = min(TP_avg, na.rm = TRUE),
                   TP_max = max(TP_avg, na.rm = TRUE),
                   CHLA_min = min(CHLA_avg, na.rm = TRUE),
                   CHLA_max = max(CHLA_avg, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(DATE = as.Date(paste0(2021, "/", MONTH, "/", 01)))

## 03.1 monthly-boxplot-w-current-YR -----------------------------------------

### chlorophyll a ----
# pellicer creek  
pc_chla_boxplot <- ggplot(data = NUT_monthly, 
       aes(x = MONTH_abb, y = CHLA_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmpcnut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmpcnut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 4.3, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
       title = "Pellicer Creek") +
  annotate("text",
           x = "Mar",
           y = 38,
           size = 3,
           color = "blue",
           label = "State Threshold 4.3 (\U00B5g/L)")

# pine island
pi_chla_boxplot <- ggplot(data = NUT_monthly, 
       aes(x = MONTH_abb, y = CHLA_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmpinut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmpinut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 6.6, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
       title = "Pine Island") +
  annotate("text",
           x = "Mar",
           y = 16,
           size = 3,
           color = "blue",
           label = "State Threshold 6.6 (\U00B5g/L)")

# san sebastian
ss_chla_boxplot <- ggplot(data = NUT_monthly, 
       aes(x = MONTH_abb, y = CHLA_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmssnut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmssnut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 4.0, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
       title = "San Sebastian") +
  annotate("text",
           x = "Mar",
           y = 16,
           size = 3,
           color = "blue",
           label = "State Threshold 4.0 (\U00B5g/L)")

# fort matanzas
fm_chla_boxplot <- ggplot(data = NUT_monthly, 
       aes(x = MONTH_abb, y = CHLA_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmfmnut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmfmnut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 5.5, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
       title = "Fort Matanzas") +
  annotate("text",
           x = "Mar",
           y = 24,
           size = 3,
           color = "blue",
           label = "State Threshold 5.5 (\U00B5g/L)")

((pi_chla_boxplot + 
    theme(axis.text.x = element_blank(),
          legend.position = "none")) + 
    (ss_chla_boxplot + 
       theme(axis.text.x = element_blank(),
          legend.position = "none") +
       labs(y = ""))) / 
  (pc_chla_boxplot + 
     (fm_chla_boxplot +
     labs(y = "")))

### total nitrogen ----
# pellicer creek  
pc_TN_boxplot <- ggplot(data = NUT_monthly, 
                          aes(x = MONTH_abb, y = TN_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmpcnut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmpcnut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 1.10, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Total Nitrogen (mg/L)",
       title = "Pellicer Creek") +
  annotate("text",
           x = "Mar",
           y = 1.7,
           size = 3,
           color = "blue",
           label = "State Threshold 1.10 (mg/L)")

# pine island
pi_TN_boxplot <- ggplot(data = NUT_monthly, 
                          aes(x = MONTH_abb, y = TN_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmpinut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmpinut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 0.65, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Total Nitrogen (mg/L)",
       title = "Pine Island") +
  annotate("text",
           x = "Mar",
           y = 1.2,
           size = 3,
           color = "blue",
           label = "State Threshold 0.65 (mg/L)")

# san sebastian
ss_TN_boxplot <- ggplot(data = NUT_monthly, 
                          aes(x = MONTH_abb, y = TN_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmssnut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmssnut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 0.55, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Total Nitrogen (mg/L)",
       title = "San Sebastian") +
  annotate("text",
           x = "Mar",
           y = 1.0,
           size = 3,
           color = "blue",
           label = "State Threshold 0.55 (mg/L)")

# fort matanzas
fm_TN_boxplot <- ggplot(data = NUT_monthly, 
                          aes(x = MONTH_abb, y = TN_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmfmnut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmfmnut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 0.53, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Total Nitrogen (mg/L)",
       title = "Fort Matanzas") +
  annotate("text",
           x = "Mar",
           y = 1.0,
           size = 3,
           color = "blue",
           label = "State Threshold 0.55 (mg/L)")

((pi_TN_boxplot + 
    theme(axis.text.x = element_blank(),
          legend.position = "none")) + 
    (ss_TN_boxplot + 
       theme(axis.text.x = element_blank(),
             legend.position = "none") +
       labs(y = ""))) / 
  (pc_TN_boxplot + 
     (fm_TN_boxplot +
        labs(y = "")))

### total phosphorus ----
# pellicer creek  
pc_TP_boxplot <- ggplot(data = NUT_monthly, 
                        aes(x = MONTH_abb, y = TP_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmpcnut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmpcnut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 0.123, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Total Phosphorus (mg/L)",
       title = "Pellicer Creek") +
  annotate("text",
           x = "Mar",
           y = 0.25,
           size = 3,
           color = "blue",
           label = "State Threshold 0.123 (mg/L)")

# pine island
pi_TP_boxplot <- ggplot(data = NUT_monthly, 
                        aes(x = MONTH_abb, y = TP_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmpinut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmpinut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 0.105, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Total Phosphorus (mg/L)",
       title = "Pine Island") +
  annotate("text",
           x = "Mar",
           y = 0.15,
           size = 3,
           color = "blue",
           label = "State Threshold 0.105 (mg/L)")

# san sebastian
ss_TP_boxplot <- ggplot(data = NUT_monthly, 
                        aes(x = MONTH_abb, y = TP_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmssnut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmssnut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 0.11, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Total Phosphorus (mg/L)",
       title = "San Sebastian") +
  annotate("text",
           x = "Mar",
           y = 0.12,
           size = 3,
           color = "blue",
           label = "State Threshold 0.11 (mg/L)")

# fort matanzas
fm_TP_boxplot <- ggplot(data = NUT_monthly, 
                        aes(x = MONTH_abb, y = TP_avg)) +
  geom_boxplot(data = filter(NUT_monthly, STATION_CODE == "gtmfmnut" & YEAR < 2021), 
               aes(fill = "2002-2020")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmfmnut" & YEAR == 2021), 
             aes(color = "2021"),
             size = 4) +
  geom_hline(yintercept = 0.111, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", 
                     values = c("2021" = "red")) +
  scale_fill_manual(name = "",
                    values = c("2002-2020" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Monthly Total Phosphorus (mg/L)",
       title = "Fort Matanzas") +
  annotate("text",
           x = "Mar",
           y = 0.12,
           size = 3,
           color = "blue",
           label = "State Threshold 0.111 (mg/L)")

((pi_TP_boxplot + 
    theme(axis.text.x = element_blank(),
          legend.position = "none")) + 
    (ss_TP_boxplot + 
       theme(axis.text.x = element_blank(),
             legend.position = "none") +
       labs(y = ""))) / 
  (pc_TP_boxplot + 
     (fm_TP_boxplot +
        labs(y = "")))



## 03.2 historic-ribbon-w-current-yr ---------------------------------------

# pellicer creek
ggplot() +
  geom_ribbon(data = filter(hist_minmax, STATION_CODE == "gtmpcnut"), 
              aes(x = DATE, ymin= CHLA_min, ymax= CHLA_max, fill = "Min-Max (2002-2021)")) +
  geom_point(data = filter(NUT_monthly, STATION_CODE == "gtmpcnut" & YEAR == 2021),
             aes(x = DATE, y = CHLA_avg, color = "Current Year (2021)"), 
             size = 3) +
  geom_line(data = filter(NUT_monthly, STATION_CODE == "gtmpcnut" & YEAR == 2021),
            aes(x = DATE, y = CHLA_avg)) +
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  geom_hline(yintercept = 4.3, color = "blue",
             linetype = "dashed") +
  scale_fill_manual(name = "",
                    values = c("Current Year (2021)" = "red",
                               "Min-Max (2002-2021)" = "lightgray")) +
  scale_color_manual(name = "",
                     values = c("Current Year (2021)" = "red",
                                "Min-Max (2002-2021)" = "lightgray")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12, color = "black")) +
  labs(x = "",
       y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
       title = "Pellicer Creek") +
  annotate("text",
           x = as.Date("2021-02-01"),
           y = 38,
           size = 3.5, 
           color = "blue",
           label = "State Threshold 4.3 (\U00B5g/L)")


## 03.3 all with trend -----------------------------------------------------

ggplot(data = NUT_monthly, aes(x = DATE, y = TP_avg, 
                               group = STATION_CODE, color = STATION_CODE)) +
  geom_smooth(aes(color = STATION_CODE), method = "lm", se = F) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_color_brewer(name = "Station", type = "qual", palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, 12, by = 1)) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
       title = "All Stations") +
  annotate("text",
           x = as.Date("2021-09-01"),
           y = 5.2,
           size = 10,
           color = "red",
           label = "*") +
  annotate("text",
           x = as.Date("2021-09-01"),
           y = 4.35,
           size = 10,
           color = "red",
           label = "*")


### monthly with trendline ----

#### fecal ----
pc <- ggplot(data = filter(NUT_monthly, STATION_CODE == "gtmpcnut" & !is.na(FECAL_avg)), 
             aes(x = DATE, y = FECAL_avg)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = F) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Mean Monthly Fecal Coliforms (CFU)",
       title = "Pellicer Creek")


pi <- ggplot(data = filter(NUT_monthly, STATION_CODE == "gtmpinut" & !is.na(FECAL_avg)), 
             aes(x = DATE, y = FECAL_avg)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = F) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 50)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Mean Monthly Fecal Coliforms (CFU)",
       title = "Pine Island")

ss <- ggplot(data = filter(NUT_monthly, STATION_CODE == "gtmssnut" & !is.na(FECAL_avg)), 
             aes(x = DATE, y = FECAL_avg)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = F) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Mean Monthly Fecal Coliforms (CFU)",
       title = "San Sebastian")

fm <- ggplot(data = filter(NUT_monthly, STATION_CODE == "gtmfmnut" & !is.na(FECAL_avg)), 
             aes(x = DATE, y = FECAL_avg)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = F) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 50)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Mean Monthly Fecal Coliforms (CFU)",
       title = "Fort Matanzas")

#### chla, TN, TP ----
pi <- ggplot(data = filter(NUT_monthly, STATION_CODE == "gtmpinut"), 
             aes(x = DATE, y = TP_avg)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = F) +
  geom_hline(yintercept = 0.105, color = "blue",
             linetype = "dashed") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Mean Monthly Total Phosphorus (mg/L)",
       title = "Pine Island") +
  annotate("text",
           x = as.Date("2006-01-01"),
           y = 0.16,
           size = 3,
           color = "blue",
           label = "State Threshold 0.105 (mg/L)")

ss <- ggplot(data = filter(NUT_monthly, STATION_CODE == "gtmssnut"), 
             aes(x = DATE, y = TP_avg)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = F) +
  geom_hline(yintercept = 0.11, color = "blue",
             linetype = "dashed") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Mean Monthly Total Phosphorus (mg/L)",
       title = "San Sebastian") +
  annotate("text",
           x = as.Date("2006-01-01"),
           y = 0.124,
           size = 3,
           color = "blue",
           label = "State Threshold 0.11 (mg/L)")

fm <- ggplot(data = filter(NUT_monthly, STATION_CODE == "gtmfmnut"), 
             aes(x = DATE, y = TP_avg)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = F) +
  geom_hline(yintercept = 0.111, color = "blue",
             linetype = "dashed") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Mean Monthly Total Phosphorus (mg/L)",
       title = "Fort Matanzas") +
  annotate("text",
           x = as.Date("2006-01-01"),
           y = 0.14,
           size = 3,
           color = "blue",
           label = "State Threshold 0.111 (mg/L)")

((pi + 
    theme(axis.text.x = element_blank(),
          legend.position = "none")) + 
    (ss + 
       theme(axis.text.x = element_blank(),
             legend.position = "none") +
       labs(y = ""))) / 
  (pc + 
     (fm +
        labs(y = "")))

# 04 yearly ---------------------------------------------------------------
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

## 04.1 yearly-agm-lines --------------------------------------------------

### chlorophyll ----
# pellicer creek
pc_chla_agm <- NUT_yearly  %>% 
  dplyr::filter(STATION_CODE == "gtmpcnut") %>% 
ggplot(aes(x = YEAR, y = CHLA_agm, group = 1)) +
  geom_line() +
  geom_point(aes(color = CHLA_agm > 4.3), size = 3) +
  geom_hline(yintercept = 4.3, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", values = c("black", "red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Geo.Mean Annual Chlorophyll-a (\U00B5g/L)",
       title = "Pellicer Creek") +
  annotate("text",
           x = "2006",
           y = 16,
           size = 3,
           color = "blue",
           label = "State Threshold 4.3 (\U00B5g/L)")

# pine island
pi_chla_agm <- NUT_yearly  %>% 
  dplyr::filter(STATION_CODE == "gtmpinut") %>% 
  ggplot(aes(x = YEAR, y = CHLA_agm, group = 1)) +
  geom_line() +
  geom_point(aes(color = CHLA_agm > 6.6), size = 3) +
  geom_hline(yintercept = 6.6, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", values = c("black", "red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Geo.Mean Annual Chlorophyll-a (\U00B5g/L)",
       title = "Pine Island") +
  annotate("text",
           x = "2006",
           y = 8,
           size = 3,
           color = "blue",
           label = "State Threshold 6.6 (\U00B5g/L)")

# san sebastian
ss_chla_agm <- NUT_yearly  %>% 
  dplyr::filter(STATION_CODE == "gtmssnut") %>% 
  ggplot(aes(x = YEAR, y = CHLA_agm, group = 1)) +
  geom_line() +
  geom_point(aes(color = CHLA_agm > 4.0), size = 3) +
  geom_hline(yintercept = 4.0, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", values = c("black", "red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Geo.Mean Annual Chlorophyll-a (\U00B5g/L)",
       title = "San Sebastian") +
  annotate("text",
           x = "2006",
           y = 6,
           size = 3,
           color = "blue",
           label = "State Threshold 4.0 (\U00B5g/L)")

# fort matanzas
fm_chla_agm <- NUT_yearly  %>% 
  dplyr::filter(STATION_CODE == "gtmfmnut") %>% 
  ggplot(aes(x = YEAR, y = CHLA_agm, group = 1)) +
  geom_line() +
  geom_point(aes(color = CHLA_agm > 5.5), size = 3) +
  geom_hline(yintercept = 5.5, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", values = c("black", "red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Geo.Mean Annual Chlorophyll-a (\U00B5g/L)",
       title = "Fort Matanzas") +
  annotate("text",
           x = "2006",
           y = 6,
           size = 3,
           color = "blue",
           label = "State Threshold 5.5 (\U00B5g/L)")
((pi_chla_agm + 
    theme(axis.text.x = element_blank(),
          legend.position = "none")) + 
    (ss_chla_agm + 
       theme(axis.text.x = element_blank(),
             legend.position = "none") +
       labs(y = ""))) / 
  (pc_chla_agm + 
     (fm_chla_agm +
        labs(y = "")))

### tn & tp ----
# pellicer creek
pc_tn_agm <- NUT_yearly  %>% 
  dplyr::filter(STATION_CODE == "gtmpcnut") %>% 
  ggplot(aes(x = YEAR, y = TP_agm, group = 1)) +
  geom_line() +
  geom_point(aes(color = TP_agm > 0.123), size = 3) +
  geom_hline(yintercept = 0.123, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", values = c("black", "red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Geo.Mean Annual Total Phosphorus (mg/L)",
       title = "Pellicer Creek") +
  annotate("text",
           x = "2006",
           y = 0.13,
           size = 3,
           color = "blue",
           label = "State Threshold 0.123 (mg/L)")

# pine island
pi_tn_agm <- NUT_yearly  %>% 
  dplyr::filter(STATION_CODE == "gtmpinut") %>% 
  ggplot(aes(x = YEAR, y = TP_agm, group = 1)) +
  geom_line() +
  geom_point(aes(color = TP_agm > 0.105), size = 3) +
  geom_hline(yintercept = 0.105, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", values = c("black", "red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Geo.Mean Annual Total Phosphorus (mg/L)",
       title = "Pine Island") +
  annotate("text",
           x = "2006",
           y = 0.11,
           size = 3,
           color = "blue",
           label = "State Threshold 0.105 (mg/L)")

# san sebastian
ss_tn_agm <- NUT_yearly  %>% 
  dplyr::filter(STATION_CODE == "gtmssnut") %>% 
  ggplot(aes(x = YEAR, y = TP_agm, group = 1)) +
  geom_line() +
  geom_point(aes(color = TP_agm > 0.11), size = 3) +
  geom_hline(yintercept = 0.11, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", values = c("black", "red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Geo.Mean Annual Total Phosphorus (mg/L)",
       title = "San Sebastian") +
  annotate("text",
           x = "2006",
           y = 0.13,
           size = 3,
           color = "blue",
           label = "State Threshold 0.11 (mg/L)")

# fort matanzas
fm_tn_agm <- NUT_yearly  %>% 
  dplyr::filter(STATION_CODE == "gtmfmnut") %>% 
  ggplot(aes(x = YEAR, y = TP_agm, group = 1)) +
  geom_line() +
  geom_point(aes(color = TP_agm > 0.111), size = 3) +
  geom_hline(yintercept = 0.111, color = "blue",
             linetype = "dashed") +
  scale_color_manual(name = "", values = c("black", "red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.6)) +
  labs(x = "",
       y = "Geo.Mean Annual Total Phosphorus (mg/L)",
       title = "Fort Phosphorus") +
  annotate("text",
           x = "2006",
           y = 0.12,
           size = 3,
           color = "blue",
           label = "State Threshold 0.111 (mg/L)")


((pi_tn_agm + 
    theme(axis.text.x = element_blank(),
          legend.position = "none")) + 
    (ss_tn_agm + 
       theme(axis.text.x = element_blank(),
             legend.position = "none") +
       labs(y = ""))) / 
  (pc_tn_agm + 
     (fm_tn_agm +
        labs(y = "")))
