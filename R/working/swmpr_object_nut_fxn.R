swmpr_sd <- function(stat_in, meta_in){
  
  if(!is.data.frame(stat_in)) 
    stop('stat_in must be data.frame')
  
  # qaqc attribute
  qaqc_cols <- FALSE
  if(any(grepl('^F_', names(stat_in)))) qaqc_cols <- TRUE
  
  # cens attribute
  cens_cols <- FALSE
  if(any(grepl('^c_', names(stat_in)))) cens_cols <- TRUE
  
  # parameters attribute
  parameters <- grep('DATE_TIME_STAMP|^F_|^C_', names(stat_in), invert = TRUE, value = TRUE)
  
  # get stations, param_types attributes
  param_types <- param_names()
  param_types <- unlist(lapply(param_types, function(x) any(x %in% parameters)))
  param_types <- names(param_names())[param_types]
  station <- grep(paste0(param_types, collapse = '|'), meta_in, value = TRUE)
  
  # remove trailing blanks in qaqc columns
  if(qaqc_cols){
    
    fcols <- grep('^f_', names(stat_in),value = TRUE)
    stat_in[, fcols] <- sapply(fcols, function(x){
      
      out <- trimws(stat_in[, x], which = 'right') #gsub('\\s+$', '', stat_in[, x])
      return(out)
      
    })
    
  }
  
  # timezone using time_vec function
  timezone <- time_vec(STATION_CODE = STATION, tz_only = TRUE)
  
  # create class, with multiple attributes
  structure(
    .Data = stat_in, 
    class = c('swmpr', 'data.frame'), 
    station = station,
    parameters = parameters, 
    qaqc_cols = qaqc_cols,
    cens_cols = cens_cols, 
    date_rng = range(stat_in$datetimestamp),
    timezone = timezone, 
    stamp_class = class(stat_in$datetimestamp),
    metabolism = NULL, 
    metab_units = NULL
  )
  
}


