# load data
source(here::here('R', '00_loadpackages.R'))
source(here::here('R', '02.1_load_wrangle_NUT.R'))
source(here::here('R', 'working', 'N_P_forms_statistics.R'))

nitro_owqtf <- nitro1 %>% 
  select(STATION_CODE, DATE, DON, DIN, PN, TN) %>% 
  filter(PN > 0) %>% 
  mutate(DON_pct = (DON/TN)*100,
         DIN_pct = (DIN/TN) *100,
         PN_pct = (PN/TN)*100) %>% 
  select(STATION_CODE, DATE, 7:9) %>% 
  pivot_longer(3:5,
               names_to = "nitro",
               values_to = "percent") %>% 
  mutate(STATION_CODE = factor(STATION_CODE,
                               levels = c("gtmpinut",
                                         "gtmssnut",
                                         "gtmfmnut",
                                         "gtmpcnut"),
                               labels = c("Pine Island",
                                          "San Sebastian",
                                          "Fort Matanzas",
                                          "Pellicer Creek")))

nitro_owqtf %>% 
  ggplot(aes(x = DATE)) +
  geom_point(aes(y = percent, color = nitro), size = 3) +
  geom_line(aes(y = percent, color = nitro)) +
  facet_wrap(~STATION_CODE, ncol=1) +
  scale_color_okabeito() +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 18),
        axis.title.y = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18)) +
  labs(x = "",
       y = "Percent of Total Nitrogen")