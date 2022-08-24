# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## 01.1 2021 field data loop ------------------------------------------------------

# initialize readin listing
mysheets_fromexcel <- list()

# get the list of all the sheet names
mysheetlist <- readxl::excel_sheets(path = here::here('data', 
                                                      '2021',
                                                      '2021_FIELDDATA_v1.xlsx'))

# create loop for the sheets
i = 1

for (i in 1:length(mysheetlist)){
  
  tempdf <- readxl::read_excel(path = here::here('data', 
                                                 '2021',
                                                 '2021_FIELDDATA_v1.xlsx'), 
                               sheet = mysheetlist[i])
  
  tempdf$sheetname <- mysheetlist[i]
  
  mysheets_fromexcel[[i]] <- tempdf 
}

mysheets_fromexcel


# merge all the lists into one tibble using dplyr::bind_rows()
env <- purrr::reduce(mysheets_fromexcel, dplyr::bind_rows) %>% 
  janitor::clean_names()

# inspect the data
glimpse(env)

# clear environment

rm(mysheets_fromexcel, 
   tempdf, i, mysheetlist)

# 02 wrangle-tidy field data ---------------------------------------------

env2 <- env %>%
  dplyr::mutate(date_sampled = lubridate::ymd_hm(paste(date, time_24_hr)),
                site = tolower(site),
                component_long = tolower(component_long),
                cdmo_name = forcats::as_factor(component_short)
                ) 
## 02.2 wide-format for export ---------------------------------------------

env_wide_result <- env2 %>% 
  dplyr::select(station_code, date_sampled, cdmo_name, component_long, result) %>%
  tidyr::pivot_wider(id_cols = c("station_code", "date_sampled"),
                     names_from = cdmo_name,
                     values_from = result)
  
env_wide_remark <- env2 %>% 
  dplyr::select(station_code, date_sampled, cdmo_name, component_long, remark) %>%
  dplyr::mutate(cdmo_name = paste0("F_", cdmo_name)) %>% 
  tidyr::pivot_wider(id_cols = c("station_code", "date_sampled"),
                     names_from = cdmo_name,
                     values_from = remark) 

env_wide <- env_wide_result %>% 
  dplyr::left_join(env_wide_remark, by = c("station_code", "date_sampled")) %>% 
  dplyr::mutate(fullstationname = station_code) %>% 
  dplyr::rename(datetimestamp = date_sampled) %>% 
  tidyr::separate(station_code, 
                  into = c("station_code", "num"), 
                  sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  tidyr::separate(num,
                  into = c("monitoringprogram", "rep"),
                  sep = "[.]") %>% 
  dplyr::select(station_code, fullstationname, datetimestamp, monitoringprogram, rep,
                Depth, F_Depth,
                Depth_S, F_Depth_S,
                SECCHI, F_SECCHI,
                WIND_S, F_WIND_S,
                WIND_D, F_WIND_D,
                ATEMP, F_ATEMP,
                WTEM_T, F_WTEM_T,
                SpCond_T, F_SpCond_T,
                pH_T, F_pH_T,
                DO_T, F_DO_T,
                SALT_T, F_SALT_T,
                WTEM_B, F_WTEM_B,
                SpCond_B, F_SpCond_B,
                pH_B, F_pH_B,
                DO_B, F_DO_B,
                SALT_B, F_SALT_B)

## clean up environment
rm(env_wide_remark, env_wide_result)

# 03 export field data for QAQC ----------------------------------------------

# remove the NAs that will come through on file export
env_wide <- sapply(env_wide, as.character)
env_wide[is.na(env_wide)] <- " "
env_wide <- as.data.frame(env_wide)

# careful, if running this code twice in the same day, you will get a warning that a sheet of that name already exists. 
# may need to delete sheet of same date if you ran into error
xlsx::write.xlsx(env_wide, here::here('output', 'field-data_wide.xlsx'),
                 sheetName = paste(Sys.Date()), # change this date
                 append = TRUE)
