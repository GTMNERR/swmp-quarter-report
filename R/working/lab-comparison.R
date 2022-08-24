# install.packages(c('jtools', 'interactions'))
library(jtools)
library(interactions)

dat <- NUT %>%
          select(1:7, 16:23, 38:41, 48:49) %>% 
          mutate(LAB = case_when(DATE_TIME_STAMP < "2012-11-30 23:45:00" ~ "UF",
                                 DATE_TIME_STAMP > "2012-11-30 23:45:00" ~ "FDEP")) 

# CHLA ----
dat_chla <- dat %>% 
  dplyr::select(1:4, CHLA_N, F_CHLA_N, LAB) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_CHLA_N) | is.na(F_CHLA_N)) %>% 
  dplyr::select(-F_CHLA_N) %>% 
  # dplyr::filter(MONITORING_PROGRAM == 1) %>%
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP)

## CHLA figures ----
dat_chla %>% 
  drop_na(LAB) %>% 
  ggplot(aes(x = DATE, y = CHLA_N, color = LAB)) +
  geom_point() +
  theme_classic()

dat_chla %>% 
  drop_na(LAB) %>% 
  ggplot(aes(x = sqrt(CHLA_N), fill = LAB)) +
  geom_histogram() +
  facet_wrap(~LAB)

boxplot(sqrt(CHLA_N) ~ LAB, dat_chla)

## CHLA analyses ----
chla_out <- aov(sqrt(CHLA_N) ~ LAB, dat_chla)

plot(chla_out)
summary(chla_out)
broom::tidy(chla_out)
broom::glance(chla_out)

# TSS ----
dat_tss <- dat %>% 
  dplyr::select(1:4, TSS, F_TSS, LAB) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_TSS) | is.na(F_TSS)) %>%
  dplyr::filter(!is.na(TSS)) %>% # remove any missing TSS entries
  dplyr::select(-F_TSS) %>% 
  # dplyr::filter(MONITORING_PROGRAM == 1) %>%
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP)

dat_tss %>% 
  drop_na(LAB) %>% 
  ggplot(aes(x = DATE, y = TSS, color = LAB)) +
  geom_point()

dat_tss %>% 
  drop_na(LAB) %>% 
  ggplot(aes(x = log(TSS), fill = LAB)) +
  geom_histogram() +
  facet_wrap(~LAB)

tss_lab <- dat_tss %>% 
  lm(formula = TSS ~ LAB)

tss_lab2 <- dat_tss %>% 
  lm(formula = log(TSS) ~ LAB)

boxplot(log(TSS) ~ LAB, dat_tss)

broom::tidy(tss_lab2)
broom::glance(tss_lab)
plot(tss_lab2)

tss_lab2_1 <- aov(log(TSS) ~ LAB, dat_tss)
plot(tss_lab2_1)
summary(tss_lab2_1)

# NH3 ----
