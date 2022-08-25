# 01 Nutrients ---------------------------------------------------------------

## 01.1 monthly-boxplot-w-current-YR ----
# more flexible version of boxplot function created 2022-08-25
boxplot_currentyear <- function(station, param, threshold, thres_val) {
  # parameters: station, param, threshold, thres_val
  
  if (threshold == TRUE) {
    boxplot <- ggplot(data = NUT_monthly, 
                      aes(x = month_abb, y = {{param}})) +
      geom_boxplot(data = filter(NUT_monthly, station_code == station & year < 2022), 
                   aes(fill = "2002-2021")) +
      geom_point(data = filter(NUT_monthly, station_code == station & year == 2022), 
                 aes(color = "2022"),
                 size = 4) +
      geom_hline(yintercept = thres_val, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", 
                         values = c("2022" = "red")) +
      scale_fill_manual(name = "",
                        values = c("2002-2021" = "white")) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.text = element_text(color = "black")) +
      labs(x = "")
    boxplot
  } else {
    boxplot <- ggplot(data = NUT_monthly, 
                      aes(x = month_abb, y = {{param}})) +
      geom_boxplot(data = filter(NUT_monthly, station_code == station & year < 2022), 
                   aes(fill = "2002-2021")) +
      geom_point(data = filter(NUT_monthly, station_code == station & year == 2022), 
                 aes(color = "2022"),
                 size = 4) +
      scale_color_manual(name = "", 
                         values = c("2022" = "red")) +
      scale_fill_manual(name = "",
                        values = c("2002-2021" = "white")) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.text = element_text(color = "black")) +
      labs(x = "")
    boxplot
  }
}


## 01.2 yearly agms ----

agm <- function(station, param, threshold, thres_val) {
  # label y axis with ex: "Geo.Mean Annual Chlorophyll-a (\U00B5g/L)"
  
  if (threshold == TRUE) {
    agm <- NUT_yearly  %>% 
      dplyr::filter(station_code == station) %>% 
      ggplot(aes(x = year, y = {{param}}, group = 1)) +
      geom_line() +
      geom_point(aes(color = {{param}} > thres_val), size = 3) +
      geom_hline(yintercept = thres_val, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", values = c("black", "red")) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_text(color = "black"),
            axis.text.x = element_text(angle = 45, 
                                       vjust = 0.6)) +
      labs(x = "")
    agm
  } else {
    agm <- NUT_yearly  %>% 
      dplyr::filter(station_code == station) %>% 
      ggplot(aes(x = year, y = {{param}}, group = 1)) +
      geom_line() +
      geom_point(size = 3) +
      scale_color_manual(name = "", values = c("black", "red")) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_text(color = "black"),
            axis.text.x = element_text(angle = 45, 
                                       vjust = 0.6)) +
      labs(x = "")
    agm
  }
}


