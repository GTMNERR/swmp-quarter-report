# 01 Nutrients ---------------------------------------------------------------

## 01.1 monthly-boxplot-w-current-YR ----

boxplot_currentyear <- function(station, param, threshold) {
  # parameters: station, param, threshold
  # 0 is CHLA_avg, 1 is TN_avg, 2 is TP_avg
  
  if (param == 0) {  
    boxplot <- ggplot(data = NUT_monthly, 
                      aes(x = month_abb, y = chla_n)) +
      geom_boxplot(data = filter(NUT_monthly, station_code == station & year < 2022), 
                   aes(fill = "2002-2021")) +
      geom_point(data = filter(NUT_monthly, station_code == station & year == 2022), 
                 aes(color = "2022"),
                 size = 4) +
      geom_hline(yintercept = threshold, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", 
                         values = c("2022" = "red")) +
      scale_fill_manual(name = "",
                        values = c("2002-2021" = "white")) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.text = element_text(color = "black")) +
      labs(x = "",
           y = "Mean Monthly Chlorophyll-a (\U00B5g/L)")
  } else if (param == 1) {
    
    boxplot <- ggplot(data = NUT_monthly, 
                      aes(x = month_abb, y = tn)) +
      geom_boxplot(data = filter(NUT_monthly, station_code == station & year < 2022), 
                   aes(fill = "2002-2021")) +
      geom_point(data = filter(NUT_monthly, station_code == station & year == 2022), 
                 aes(color = "2022"),
                 size = 4) +
      geom_hline(yintercept = threshold, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", 
                         values = c("2022" = "red")) +
      scale_fill_manual(name = "",
                        values = c("2002-2021" = "white")) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.text = element_text(color = "black")) +
      labs(x = "",
           y = "Mean Monthly Total Nitrogen (mg/L)")
    
  } else {
    boxplot <- ggplot(data = NUT_monthly, 
                      aes(x = month_abb, y = tp)) +
      geom_boxplot(data = filter(NUT_monthly, station_code == station & year < 2022), 
                   aes(fill = "2002-2021")) +
      geom_point(data = filter(NUT_monthly, station_code == station & year == 2022), 
                 aes(color = "2022"),
                 size = 4) +
      geom_hline(yintercept = threshold, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", 
                         values = c("2022" = "red")) +
      scale_fill_manual(name = "",
                        values = c("2002-2021" = "white")) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.text = element_text(color = "black")) +
      labs(x = "",
           y = "Mean Monthly Total Phosphorus (mg/L)")
  }
}

## 01.2 yearly agms ----

agm <- function(station, param, threshold) {
  if (param == 0) {
    agm <- NUT_yearly  %>% 
      dplyr::filter(station_code == station) %>% 
      ggplot(aes(x = year, y = CHLA_agm, group = 1)) +
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
      dplyr::filter(station_code == station) %>% 
      ggplot(aes(x = year, y = TN_agm, group = 1)) +
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
      dplyr::filter(station_code == station) %>% 
      ggplot(aes(x = year, y = TP_agm, group = 1)) +
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
