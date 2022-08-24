# using methods based on Cloern et al. 2020
# Nitrogen partitioning
# By SWMP Station

# load data
source(here::here('R', '00_loadpackages.R'))
source(here::here('R', '02.1_load_wrangle_NUT.R'))

# 0A 2018-present ----
## 01 pull out and QAQC each parameter, plus conversions to micro Moles ----
# keeping only data collected at DEP lab (2018-present)
NH4 <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, NH4F, F_NH4F) %>% 
        filter(!grepl("CUS|-3", F_NH4F)) %>% 
        mutate(NH4uM = NH4F * (1000/14.01))

NO23 <- NUT %>% 
          filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
          select(STATION_CODE, DATE_TIME_STAMP, NO23F, F_NO23F) %>% 
          filter(!grepl("CUS|SCC|-3", F_NO23F)) %>% 
          mutate(NO23uM = NO23F * (1000/14.01))

TKN <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, TKN, F_TKN) %>% 
        filter(!grepl("CUS|GQS|SCC|-3", F_TKN)) %>% 
        mutate(TKNuM = TKN * (1000/14.01))

TKNF <- NUT %>% 
          filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
          select(STATION_CODE, DATE_TIME_STAMP, TKNF, F_TKNF) %>% 
          filter(!grepl("CUS|SCC|-3", F_TKNF)) %>% 
          mutate(TKNFuM = TKNF * (1000/14.01))

CHLA <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, CHLA_N, F_CHLA_N) %>% 
        filter(!grepl("CUS|-3", F_CHLA_N))

TSS <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, TSS, F_TSS) %>% 
        filter(!grepl("CUS|CHB|-3", F_TSS))

DIP <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        filter(!grepl("2020-04-22", DATE_TIME_STAMP)) %>%
        select(STATION_CODE, DATE_TIME_STAMP, PO4F, F_PO4F) %>% 
        filter(!grepl("CUS|CHB|-3", F_PO4F)) %>% 
        rename(DIP = PO4F) %>% 
        mutate(DIPuM = DIP * (1000/30.97))

TP <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        filter(!grepl("2020-04-22", DATE_TIME_STAMP)) %>%  # remove phosphorus from 2020-04
        select(STATION_CODE, DATE_TIME_STAMP, TP, F_TP) %>% 
        filter(!grepl("CUS|-3", F_TP)) %>% 
        mutate(TPuM = TP * (1000/30.97))

SALT <- NUT %>% 
  filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
  select(STATION_CODE, DATE_TIME_STAMP, SALT_N, F_SALT_N) 

DO <- NUT %>% 
  filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
  select(STATION_CODE, DATE_TIME_STAMP, DO_N, F_DO_N) %>% 
  filter(!grepl("GQS", F_DO_N))

## 02 Merge all parameters back into one dataframe called "nitro" ----

nitro <- NH4 %>% left_join(NO23, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(TKN, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(TKNF, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(TSS, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(CHLA, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(DIP, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(TP, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(SALT, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(DO, by = c("STATION_CODE", "DATE_TIME_STAMP"))

# clean up environment
rm(NH4, NO23, TKN, TKNF, TSS, CHLA, TP, DIP, SALT, DO)

## 03 calculate DIN, TN, DON, and PN ----
# calculations of DIN, TN, DON, and PN

# need to group by station and date and then take average due to replicates 1.1 and 1.2 values
nitro1 <- nitro %>% 
            mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
            group_by(STATION_CODE, DATE) %>% 
            summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
            ungroup() %>% 
            mutate(DIN = NO23F + NH4F,
                   TN = TKN + NO23F,
                   DON = TKNF - NH4F,
                   PN = TN - (DIN + DON),
                   DINuM = NO23uM + NH4uM,
                   TNuM = TKNuM + NO23uM,
                   DONuM = TKNFuM - NH4uM,
                   PNuM = TNuM - (DINuM + DONuM)) %>% 
  mutate_all(~ifelse(is.nan(.), NA, .)) %>% # remove all NaN and replace with NA 
  mutate(DATE = as.Date(DATE)) # reformats the date data

## 04 site specific calculations ----
# separate out sites to calculate multiple linear regression of PN as functions of TSS and CHLA

            
# pine island
pi <- nitro1 %>% filter(STATION_CODE == "gtmpinut" & PNuM > 0)

pi_fit <- lm(PNuM ~ TSS + CHLA_N, data = na.exclude(pi))
summary(pi_fit)
broom::tidy(pi_fit)
broom::glance(pi_fit)

pi <- pi %>% 
  mutate(N_sed = 0.192*TSS, # get the number from the estimate in lm for TSS
         N_phyto = 0.280*CHLA_N) # get the number from the estimate in lm for CHLA

# san sebastian
ss <- nitro1 %>% filter(STATION_CODE == "gtmssnut" & PNuM > 0)

ss_fit <- lm(PNuM ~ TSS + CHLA_N, data = na.exclude(ss))
summary(ss_fit)
broom::tidy(ss_fit)

ss <- ss %>% 
  mutate(N_sed = 0.276*TSS,
         N_phyto = 0.315*CHLA_N)

# fort matanzas
fm <- nitro1 %>% filter(STATION_CODE == "gtmfmnut" & PNuM > 0)

fm_fit <- lm(PNuM ~ TSS + CHLA_N, data = na.exclude(fm))
summary(fm_fit)
broom::tidy(fm_fit)

fm <- fm %>% 
  mutate(N_sed = 0.23*TSS,
         N_phyto = 0.499*CHLA_N)
         

# pellicer creek
pc <- nitro1 %>% filter(STATION_CODE == "gtmpcnut" & PNuM > 0)

pc_fit <- lm(PNuM ~ TSS + CHLA_N, data = na.exclude(pc))
summary(pc_fit)
broom::tidy(pc_fit)

pc <- pc %>% 
  mutate(N_sed = 0.354*TSS,
         N_phyto = 0.370*CHLA_N)


# merge all sites back into one dataframe
sites <- bind_rows(pi, ss, fm, pc) %>% 
          mutate(N_limit = 100*(DINuM/(DINuM + 1.6)),
                 P_limit = 100*(DIPuM/(DIPuM + 0.24)),
                 TN_TP = TN/TP,
                 TN_TPuM = TNuM/TPuM,
                 DIN_DIPuM = DINuM/DIPuM,
                 DIN_DIP = DIN/DIP,
                 N_sed_mg = N_sed * 14.01/1000,
                 N_phyto_mg = N_phyto * 14.01/1000, 
                 N_eff = CHLA_N/DIN,
                 N_effuM = CHLA_N/DINuM,
                 P_eff = CHLA_N/DIP,
                 P_effuM = CHLA_N/DIPuM)

# clean-up environment
rm(pi, pi_fit, ss, ss_fit, fm, fm_fit, pc, pc_fit)

## 05 min max mean each parameter ----
# calculate min, max, and mean of each parameter
sites_calc <- sites %>%
  select(-DATE) %>% 
  group_by(STATION_CODE) %>% 
  summarise(across(where(is.numeric), list(min = min, max = max, med = median, mean = mean), na.rm = TRUE))

count <- sites %>% 
  group_by(STATION_CODE) %>% 
  summarise(across(everything(), ~ n()))

# replace columns in "count" with a _N to identify them as a count
colnames(count) <- paste(colnames(count), sep = "_", "N")
count <- count %>% rename(STATION_CODE = STATION_CODE_N) %>% select(-DATE_N)
all <- sites_calc %>% 
  left_join(count, by = "STATION_CODE") %>% 
  mutate(Nitrogen_eff = CHLA_N_mean/DIN_mean,
         Nitrogen_effuM = CHLA_N_mean/DINuM_mean,
         Phosphorus_eff = CHLA_N_mean/DIP_mean,
         Phosphorus_effuM = CHLA_N_mean/DIPuM_mean)

## 06 export file ----
## uncomment to export file
# write.xlsx(all, here::here("output", "data", "N_and_P_statistics.xlsx"))

## 07 plots ----

### 07a stacked bar plots ----

# nitrogen stacked graphs
# uM
sites %>% 
  select(STATION_CODE, DATE, DINuM, DONuM, PNuM) %>% 
  rename(DIN = DINuM,
         DON = DONuM,
         PN = PNuM) %>% 
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
       y = "Nitrogen (\U3BCM)")
# mgL
sites %>%
  mutate(STATION_CODE = factor(STATION_CODE,
                               levels = c("gtmpinut",
                                          "gtmssnut",
                                          "gtmfmnut",
                                          "gtmpcnut"),
                               labels = c("Pine Island",
                                          "San Sebastian",
                                          "Fort Matanzas",
                                          "Pellicer Creek"))) %>% 
  pivot_longer(cols = 3:34,
               names_to = "nitro_source",
               values_to = "conc") %>% 
  filter(!grepl("uM", nitro_source)) %>% 
  filter(nitro_source %in% c("DIN", "DON", "PN")) %>% 
  ggplot(aes(x = DATE, y = conc, fill = nitro_source, group = STATION_CODE)) +
  geom_col() +
  facet_wrap(~STATION_CODE) +
  scale_fill_okabeito(name = "") +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0,0)) +
  theme_classic() +
  labs(x = '',
       y = "Nitrogen (mg/L)")

# chla
sites %>%
  select(STATION_CODE, DATE, CHLA_N) %>% 
  mutate(STATION_CODE = factor(STATION_CODE,
                               levels = c("gtmpinut",
                                          "gtmssnut",
                                          "gtmfmnut",
                                          "gtmpcnut"),
                               labels = c("Pine Island",
                                          "San Sebastian",
                                          "Fort Matanzas",
                                          "Pellicer Creek"))) %>% 
  ggplot(aes(x = DATE, y = CHLA_N)) +
  geom_col(fill = "forestgreen") +
  facet_wrap(~STATION_CODE) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = '',
       y = "Chlorophyll-a (\U3BCg/L)")

# phos uM
sites %>%
  select(STATION_CODE, DATE, DIPuM, TPuM) %>% 
  rename(DIP = DIPuM,
         TP = TPuM) %>% 
  filter(!(STATION_CODE == "gtmpcnut" & grepl("2020-06-02", DATE))) %>% # remove phos on 2020-06-02 due to QC
  mutate(STATION_CODE = factor(STATION_CODE,
                               levels = c("gtmpinut",
                                          "gtmssnut",
                                          "gtmfmnut",
                                          "gtmpcnut"),
                               labels = c("Pine Island",
                                          "San Sebastian",
                                          "Fort Matanzas",
                                          "Pellicer Creek"))) %>% 
  ggplot(aes(x = DATE, group = STATION_CODE)) +
  geom_col(aes(y = DIP)) +
  geom_point(aes(y = TP)) +
  geom_line(aes(y = TP)) +
  facet_wrap(~STATION_CODE) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = '',
       y = "Phosphorus (\U3BCM)",
       caption = "Bars are DIP and points represent TP.")

# phos mgL
sites %>%
  select(STATION_CODE, DATE, DIP, TP) %>%
  filter(!(STATION_CODE == "gtmpcnut" & grepl("2020-06-02", DATE))) %>% # remove phos on 2020-06-02 due to QC
  mutate(STATION_CODE = factor(STATION_CODE,
                               levels = c("gtmpinut",
                                          "gtmssnut",
                                          "gtmfmnut",
                                          "gtmpcnut"),
                               labels = c("Pine Island",
                                          "San Sebastian",
                                          "Fort Matanzas",
                                          "Pellicer Creek"))) %>% 
  ggplot(aes(x = DATE, group = STATION_CODE)) +
  geom_col(aes(y = DIP)) +
  geom_point(aes(y = TP)) +
  geom_line(aes(y = TP)) +
  facet_wrap(~STATION_CODE) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = '',
       y = "Phosphorus (mg/L)",
       caption = "Bars are DIP and points represent TP.")

# redfield ratios
sites %>% 
  select(STATION_CODE, DATE, TN_TP, DIN_DIP) %>% 
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
  geom_boxplot(aes(x = STATION_CODE, y = TN_TP, fill = STATION_CODE)) +
  geom_hline(yintercept = 7.2) + 
  scale_fill_manual(values = c("PI" = "#0072B2",
                               "SS" = "#CC7987",
                               "FM" = "#009E73",
                               "PC" = "#E69F00")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20)) +
  labs(x = '',
       y = "TN:TP")

sites %>% 
  select(STATION_CODE, DATE, TN_TP, DIN_DIP) %>% 
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
  geom_point(aes(x = DATE, y = TN_TP, color = STATION_CODE), size = 3) +
  geom_hline(yintercept = 7.2) + 
  scale_color_manual(name = "Site", 
                     values = c("PI" = "#0072B2",
                               "SS" = "#CC7987",
                               "FM" = "#009E73",
                               "PC" = "#E69F00")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size= 18),
        axis.title.y = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 18)) +
  labs(x = '',
       y = "TN:TP")

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
  geom_point(aes(x = TP, y = TN, color = STATION_CODE), color = "white", size = 3) +
  geom_abline(slope = 7.23, size = 0.5) +
  scale_color_manual(name = "Site",
                     values = c("PI" = "#0072B2",
                                "SS" = "#CC7987",
                                "FM" = "#009E73",
                                "PC" = "#E69F00")) +
  theme_classic() +
  theme(axis.text = element_text(color = "white"),
        axis.title.y = element_text(color = "black", size = 20),
        axis.title.x = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 18)) +
  labs(x = 'Total Phosphorus (mg/L)',
       y = "Total Nitrogen (mg/L)")
# boxplots

# boxplot <- function(x) {
#   
#   x <- sym(x)
#   
# sites %>% 
#   mutate(STATION_CODE = factor(STATION_CODE,
#                                levels = c("gtmpinut",
#                                           "gtmssnut",
#                                           "gtmfmnut",
#                                           "gtmpcnut"),
#                                labels = c("Pine Island",
#                                           "San Sebastian",
#                                           "Fort Matanzas",
#                                           "Pellicer Creek"))) %>% 
#   ggplot(aes(x = STATION_CODE, y = !!x)) +
#   geom_boxplot(aes(fill = STATION_CODE), alpha = 0.8) +
#     theme_classic() +
#     theme(legend.position = "") +
#     labs(x = "Site")
# }




# # 0B 2003 - 2009 ----
# 
# # reset R environment and load data again
# rm(list = ls())
# source(here::here('R', '02.1_load_wrangle_NUT.R'))
# 
# NH4 <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, NH4F, F_NH4F) %>%
#   filter(!grepl("SBL", F_NH4F)) %>%
#   mutate(NH4uM = NH4F * (1000/14.01))
# 
# NO23 <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, NO23F, F_NO23F) %>%
#   filter(!grepl("CHB|SCC|SBL", F_NO23F)) %>%
#   mutate(NO23uM = NO23F * (1000/14.01))
# 
# TN <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, TN, F_TN) %>%
#   filter(!grepl("SCC|CHB|SBL", F_TN)) %>%
#   mutate(TNuM = TN * (1000/14.01))
# 
# TDN <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, TDN, F_TDN) %>%
#   filter(!grepl("SCC|CHB|SBL", F_TDN)) %>%
#   mutate(TDNuM = TDN * (1000/14.01))
# 
# DIP <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, PO4F, F_PO4F) %>%
#   filter(!grepl("SBL", F_PO4F)) %>%
#   rename(DIP = PO4F) %>%
#   mutate(DIPuM = DIP * (1000/30.97))
# 
# TP <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, TP, F_TP) %>%
#   filter(!grepl("SCC|CHB|SBL", F_TP)) %>%
#   mutate(TPuM = TP * (1000/30.97))
# 
# TDP <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, TDP, F_TDP) %>%
#   filter(!grepl("SCC|CHB|SBL", F_TDP)) %>%
#   mutate(TDPuM = TDP * (1000/30.97))
# 
# CHLA <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP< "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, CHLA_N, F_CHLA_N) %>%
#   filter(!grepl("GQD|SBL", F_CHLA_N))
# 
# TSS <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, TSS, F_TSS) %>%
#   filter(!grepl("CHB|SBL", F_TSS))
# 
# SALT <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, SALT_N, F_SALT_N)
# 
# DO <- NUT %>%
#   filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP < "2010-01-01") %>%
#   select(STATION_CODE, DATE_TIME_STAMP, DO_N, F_DO_N)
# 
# ## 02 Merge all parameters back into one dataframe called "nitro" ----
# 
# nitro <- NH4 %>% left_join(NO23, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>%
#   left_join(TN, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>%
#   left_join(TDN, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>%
#   left_join(DIP, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>%
#   left_join(TP, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>%
#   left_join(TDP, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>%
#   left_join(TSS, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>%
#   left_join(CHLA, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>%
#   left_join(SALT, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>%
#   left_join(DO, by = c("STATION_CODE", "DATE_TIME_STAMP"))
# 
# # clean up environment
# rm(NH4, NO23, TN, TDN, TSS, CHLA, TP, DIP, TDP, SALT, DO)
# 
# ## 03 calculate DIN, TN, DON, and PN ----
# # calculations of DIN, TN, DON, and PN
# 
# nitro1 <- nitro %>%
#   mutate(DATE = as.Date(DATE_TIME_STAMP)) %>%
#   group_by(STATION_CODE, DATE) %>%
#   summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   mutate(DIN = NO23F + NH4F,
#          DON = TDN - DIN,
#          PN = TN - TDN,
#          DINuM = NO23uM + NH4uM,
#          DONuM = TDNuM - DINuM,
#          PNuM = TNuM - TDNuM,
#          PP = TP - TDP,
#          PPuM = TPuM - TDPuM) %>%
#   mutate_all(~ifelse(is.nan(.), NA, .))
# 
# ## 04 site specific calculations ----
# # separate out sites to calculate multiple linear regression of PN as functions of TSS and CHLA
# 
# 
# # pine island
# pi <- nitro1 %>% filter(STATION_CODE == "gtmpinut")
# 
# pi_fit <- lm(PNuM ~ TSS + CHLA_N, data = na.exclude(nitro1))
# summary(pi_fit)
# broom::tidy(pi_fit)
# broom::glance(pi_fit)
# 
# pi_fit2 <- lm(PPuM ~ TSS + CHLA_N, data = pi)
# summary(pi_fit2)
# broom::tidy(pi_fit2)
# 
# pi <- pi %>%
#   mutate(N_sed = -0.0212*TSS,
#          N_phyto = 0.0197*CHLA_N,
#          P_sed = 0.0055*TSS,
#          P_phyto = 0.0591*CHLA_N)
# 
# # san sebastian
# ss <- nitro1 %>% filter(STATION_CODE == "gtmssnut" & PNuM > 0)
# 
# ss_fit <- lm(PNuM ~ TSS + CHLA_N, data = ss)
# summary(ss_fit)
# broom::tidy(ss_fit)
# 
# ss <- ss %>%
#   mutate(N_sed = 0.243*TSS,
#          N_phyto = 0.381*CHLA_N)
# 
# # fort matanzas
# fm <- nitro1 %>% filter(STATION_CODE == "gtmfmnut" & PNuM > 0)
# 
# fm_fit <- lm(PNuM ~ TSS + CHLA_N, data = fm)
# summary(fm_fit)
# broom::tidy(fm_fit)
# 
# fm <- fm %>%
#   mutate(N_sed = 0.172*TSS,
#          N_phyto = 0.575*CHLA_N)
# 
# 
# # pellicer creek
# pc <- nitro1 %>% filter(STATION_CODE == "gtmpcnut" & PNuM > 0)
# 
# pc_fit <- lm(PNuM ~ TSS + CHLA_N, data = pc)
# summary(pc_fit)
# broom::tidy(pc_fit)
# 
# pc <- pc %>%
#   mutate(N_sed = 0.366*TSS,
#          N_phyto = 0.412*CHLA_N)
# 
# 
# # merge all sites back into one dataframe
# sites <- bind_rows(pi, ss, fm, pc) %>%
#   mutate(N_limit = 100*(DINuM/(DINuM + 1.6)),
#          P_limit = 100*(DIPuM/(DIPuM +0.24)),
#          TN_TPuM = TNuM/TPuM,
#          DIN_DIPuM = DINuM/DIPuM,
#          N_sed_mg = N_sed * 14.01/1000,
#          N_phyto_mg = N_phyto * 14.01/1000)
# 
# # clean-up environment
# rm(pi, pi_fit, ss, ss_fit, fm, fm_fit, pc, pc_fit)
# 
# ## 05 min max mean each parameter ----
# # calculate min, max, and mean of each parameter
# sites_calc <- sites %>%
#   group_by(STATION_CODE) %>%
#   summarise(across(where(is.numeric), list(min = min, max = max, mean = mean), na.rm = TRUE))
# 
# count <- sites %>%
#   group_by(STATION_CODE) %>%
#   summarise(across(everything(), ~ n()))
# 
# # replace columns in "count" with a _N to identify them as a count
# colnames(count) <- paste(colnames(count), sep = "_", "N")
# count <- count %>% rename(STATION_CODE = STATION_CODE_N) %>% select(-DATE_N)
# all <- sites_calc %>%
#   left_join(count, by = "STATION_CODE") %>%
#   mutate(Nitrogen_eff = CHLA_N_mean/DIN_mean,
#          Nitrogen_effuM = CHLA_N_mean/DINuM_mean,
#          Phosphorus_eff = CHLA_N_mean/DIP_mean,
#          Phosphorus_effuM = CHLA_N_mean/DIPuM_mean)
# 
# ## 06 export file ----
# write.xlsx(all, here::here("output", "data", "N_and_P_statistics.xlsx"))