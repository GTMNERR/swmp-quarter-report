# load libraries and data files 
# updated 2022-07-19 SKD to convert to swmpr object and filter using qaqc from swmpr
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## 01.1 load 2002-2023 Nutrient Data

nms <- names(read_excel(here::here('data',
                                   'swmp',
                                   'All_inclusive_NUT',
                                   'gtmnut2002-2023_QC_zeros-corrected.xlsx'), 
                        n_max = 0)) # pull out all the column names in this file

class <- ifelse(grepl("^F_", nms), "text", "numeric") # read everything with F_ as a character
class2 <- class[-(1:5)] # remove the first five elements of the vector because they are different

NUT <- readxl::read_xlsx(here::here('data',
                                    'swmp',
                                    'All_inclusive_NUT',
                                    'gtmnut2002-2023_QC_zeros-corrected.xlsx'),
                         col_types = c("text", 
                                       "date", 
                                       "numeric", 
                                       "numeric", 
                                       "text", 
                                       class2)) %>% # specify how to read in these columns
  janitor::clean_names()

# clean environment
rm(nms, class, class2)

# 02 wrangle data for merging ------------------------------------------------
NUT <- NUT %>% filter(!is.na(rep)) # remove "S" reps in dataset

# 04 wrangle to swmpr -----------------------------------------------------
# The `swmpr()` call needs to have just datetimestamp and data+qa columns, so remove the extras, while also making names lower case.
timezone <- "America/Jamaica" # needs a timezone

stations <- c("gtmpinut", "gtmssnut", "gtmfmnut", "gtmpcnut")

for (i in 1:length(stations)){
  
  tempdf <- swmpr(as.data.frame(NUT %>%
                                  filter(station_code == stations[i]) %>%
                                  select(-station_code) %>%
                                  mutate(date_time_stamp = as.POSIXct(date_time_stamp,
                                                                      tz = timezone,
                                                                      format = '%m/%d/%Y %H:%M')) %>%
                                  rename(datetimestamp = date_time_stamp,
                                         unc_chla_n = unc_ch_la_n,
                                         f_unc_chla_n = f_unc_ch_la_n) %>%
                                  filter(monitoring_program == 1) %>%
                                  select(-monitoring_program, -rep)), 
                  stations[i])
  
  # 
  name <- attr(tempdf, "station") # pull out the name you want of the file
  # 
  assign(paste0("swmp", "_", name), tempdf)
  
  rm(tempdf, name, i)
}

# check object(s) to confirm they are swmpr objects
# class(swmp_gtmpcnut)
# str(swmp_gtmpcnut)

rm(timezone, stations)

## 04.2 qaqc swmpr --------------------------------------------------------

# use the qaqc functions on the data
pi_nut <- swmp_gtmpinut %>% SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5'))
ss_nut <- swmp_gtmssnut %>% SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5'))
fm_nut <- swmp_gtmfmnut %>% SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5'))
pc_nut <- swmp_gtmpcnut %>% SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5'))

# remove unfiltered data objects
rm(swmp_gtmpinut, 
   swmp_gtmssnut, 
   swmp_gtmfmnut,
   swmp_gtmpcnut)


# 05 aggregate to monthly -------------------------------------------------

pi_nut_mo <- pi_nut %>% aggreswmp(by = "months")
ss_nut_mo <- ss_nut %>% aggreswmp(by = "months")
fm_nut_mo <- fm_nut %>% aggreswmp(by = "months")
pc_nut_mo <- pc_nut %>% aggreswmp(by = "months")

# merge together
# NUT_f <- bind_rows("gtmpinut" = pi_nut_mo, 
#                    "gtmssnut" = ss_nut_mo, 
#                    "gtmfmnut" = fm_nut_mo, 
#                    "gtmpcnut" = pc_nut_mo, 
#                    .id = "station_code")

NUT_monthly <- bind_rows("gtmpinut" = pi_nut_mo, 
                         "gtmssnut" = ss_nut_mo, 
                         "gtmfmnut" = fm_nut_mo, 
                         "gtmpcnut" = pc_nut_mo, 
                         .id = "station_code") %>%
  # select(station_code, datetimestamp, tn, tp, chla_n, tkn, po4f, din) %>% 
  dplyr::mutate(year = lubridate::year(datetimestamp), 
                month_abb = lubridate::month(datetimestamp, label = TRUE, abbr = TRUE),
                month = lubridate::month(datetimestamp),
                station_code = factor(station_code,
                                      levels = c("gtmpinut",
                                                 "gtmssnut",
                                                 "gtmfmnut",
                                                 "gtmpcnut")))

### 05.3.2 monthly averages to yearly ----
# annual geometric mean function
# gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
# or use the psych::geometric.mean() function

NUT_yearly <- NUT_monthly %>% 
  dplyr::group_by(station_code, year) %>% 
  dplyr::summarise(TN_agm = psych::geometric.mean(tn, na.rm = T),
                   TP_agm = psych::geometric.mean(tp, na.rm = T),
                   CHLA_agm = psych::geometric.mean(chla_n, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = forcats::as_factor(year))