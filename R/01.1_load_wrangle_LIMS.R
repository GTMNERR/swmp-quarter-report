# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## load LIMS File
## this file should be in the 'data/2021' folder
## if kept generic, rename file to simply 'LIMS_Download.xlsx', but if using file as is
## edit file name in import code below
lims <- readxl::read_xlsx(here::here('data', 
                                     '2021',
                                     'LIMS_Download.xlsx'), # this is where you'd want to rename the file
                          sheet = "BrowseReportPage") %>% 
        janitor::clean_names()

# inspect the data
dplyr::glimpse(lims)

# load CDMO names file
names <- readxl::read_xlsx(here::here('data', 
                                      'componentnames.xlsx')) %>%
         janitor::clean_names()

# 02 wrangle-tidy data ------------------------------------------------------
# rename some columns in lims to what we use in SWMP
# convert datetimes into POSIXct format
# make all entries in station_code and component_long columns lowercase (easier coding)
# remove field blanks
lims2 <- lims %>%
          dplyr::rename(station_code = field_id,
                        component_long = component,
                        datetimestamp = date_sampled) %>% 
          dplyr::mutate(datetimestamp = as.POSIXct(strptime(datetimestamp, 
                                                           "%d-%b-%Y %H:%M", tz='EST')),
                        date_analyzed = as.POSIXct(strptime(date_analyzed, 
                                                            "%d-%b-%Y %H:%M", tz='EST')),
                        station_code = tolower(station_code),
                        component_long = tolower(component_long)) %>% 
          dplyr::filter(station_code != "field blank") 

# correct so that TKN and TKN-F are different 
# fixing the LIMS entry so that kjeldahl nitrogen, dissolved is different from kjeldahl nitrogen. they have the same component name.
tkn_f <- lims2 %>% 
          dplyr::filter(analysis == "W-TKN-F") %>% 
          dplyr::mutate(component_long = "kjeldahl nitrogen, dissolved") 

# merge the renamed TKNF data with all the other data and then join with the `names` df to get the CDMO format names
lims3 <- lims2 %>% 
          dplyr::filter(analysis != "W-TKN-F") %>% 
          dplyr::bind_rows(tkn_f) %>%  
          dplyr::left_join(names, by = "component_long") %>% 
          dplyr::mutate(cdmo_name = forcats::as_factor(cdmo_name)) # I think conv. to factor helps with pivoting

## clean up environment ---
rm(tkn_f, lims2, names)

# 03 make LIMS data wide --------------------------------------------------
# to make the LIMS data in wide format for entry into in-house datafile

lims_wide_results <- lims3 %>% 
                        dplyr::select(station_code, datetimestamp, cdmo_name, component_long, result) %>% 
                        tidyr::pivot_wider(id_cols = c('station_code',
                                                       'datetimestamp'), 
                                           names_from = cdmo_name,
                                           values_from = result)

lims_wide_remarks <- lims3 %>% 
                        dplyr::select(station_code, datetimestamp, cdmo_name, remark) %>% 
                        dplyr::mutate(cdmo_name = paste0('F_', cdmo_name)) %>% 
                        tidyr::pivot_wider(id_cols = c('station_code',
                                                       'datetimestamp'), 
                                           names_from = cdmo_name,
                                           values_from = remark)

lims_wide <- lims_wide_results %>% 
                left_join(lims_wide_remarks, 
                          by = c("station_code", "datetimestamp")) %>%
                dplyr::mutate(fullstationname = station_code) %>% 
                tidyr::separate(station_code, 
                                into = c("station_code", "num"), 
                                sep = "(?<=[A-Za-z])(?=[0-9])") %>%
                tidyr::separate(num,
                                into = c("monitoringprogram", "rep"),
                                sep = "[.]") %>% 
                dplyr::select(1, 103, 4, 2:3, 5:102)

# to look up column numbers for easier reordering used for `select()` above          
# data.frame(colnames(lims_wide))

## clean up environment 
rm(lims_wide_remarks, lims_wide_results)


# env variables in LIMS file if wanted ----------------------------------------------

# env <- lims3 %>% 
#         select(station_code, temperature, specific_conductance, salinity, ph, dissolved_o2) %>% 
#         tidyr::separate(temperature,
#                         into = c("WTEM_N", "tempunits"),
#                         sep = "(\\s+)") %>%
#         tidyr::separate(specific_conductance,
#                         into = c("specificconductance", "spcunits"),
#                         sep = "(\\s+)") %>%
#         tidyr::separate(dissolved_o2,
#                         into = c("DO_N", "o2units"),
#                         sep = "(\\s+)") %>%
#         dplyr::select(-spcunits, -tempunits, -o2units)  
#   


# 04 export lims-wide to xlsx ------------------------------------------------

# 04.1 make file the same parameters and order as the qaqc file in-house
lims_wide2 <- lims_wide %>% 
                dplyr::mutate(F_Record = '', # build in blank columns for easier copy & paste
                              DIN = '',
                              F_DIN = '',
                              TN = '',
                              F_TN = '',
                              TON = '',
                              F_TON = '',
                              PON = '',
                              F_PON = '',
                              TDN = '',
                              F_TDN = '',
                              DON = '',
                              F_DON = '',
                              SECCHI = '',
                              F_SECCHI = '',
                              WTEM_N = '',
                              F_WTEM_N = '',
                              SALT_N = '',
                              F_SALT_N = '',
                              PH_N = '',
                              F_PH_N = '',
                              DO_N = '',
                              F_DO_N = '') %>% 
                dplyr::select(1:5, 104, # reorder everything
                              PO4F, F_PO4F,
                              TP, F_TP,
                              NH4F, F_NH4F,
                              NO2F, F_NO2F,
                              NO3F, F_NO3F,
                              NO23F, F_NO23F,
                              DIN, F_DIN,
                              TN, F_TN,
                              TKN, F_TKN,
                              TKNF, F_TKNF, 
                              TON, F_TON,
                              DON, F_DON,
                              TDN, F_TDN,
                              PON, F_PON,
                              CHLA_N, F_CHLA_N,
                              UncCHLa_N, F_UncCHLa_N,
                              PHEA, F_PHEA,
                              TSS, F_TSS,
                              TURB_N, F_TURB_N,
                              color, F_color,
                              FECCOL_CFU, F_FECCOL_CFU,
                              WTEM_N, F_WTEM_N,
                              SALT_N, F_SALT_N,
                              DO_N, F_DO_N,
                              PH_N, F_PH_N, 
                              DOC, F_DOC)


# remove the NAs that will come through on file export
lims_wide2 <- sapply(lims_wide2, as.character)
lims_wide2[is.na(lims_wide2)] <- " "
lims_wide2 <- as.data.frame(lims_wide2)

# careful, if running this code twice in the same day, you will get a warning that a sheet of that name already exists. 
# may need to delete sheet of same date if you ran into error
xlsx::write.xlsx(lims_wide2, here::here('output', 'lims_wide.xlsx'),
                 sheetName = paste(Sys.Date()), # change this date
                 append = TRUE)

