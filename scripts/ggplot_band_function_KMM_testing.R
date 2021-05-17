#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(plotly)

#----- Import the data -----
#path = "C:/Users/Diana/Documents/NETN/Water/data" #change to your path
path = "../NCRNWaterViz/Data/NETN"
netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#----- Other input functions -----
# Create percentile bands for input df
pct_fun <- function(df){
  # Check if month is a column, and add if it's not.
  df <- if(!"month" %in% names(df)){
    df %>% mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>% 
      droplevels()} else {df}
  
  df_sum <- df %>% group_by(Park, Site, Characteristic, month) %>% 
    summarize(num_samps = n(),
              median_val = median(ValueCen, na.rm = TRUE),
              lower_100 = ifelse(num_samps >= 4, min(ValueCen, na.rm = T), NA),
              upper_100 = ifelse(num_samps >= 4, max(ValueCen, na.rm = T), NA),
              lower_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.025, na.rm = T), NA),
              upper_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.975, na.rm = T), NA),
              lower_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.25, na.rm = T), NA),
              upper_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.75, na.rm = T), NA),
              mon_num = as.numeric(month),
              .groups = "drop") %>% 
    filter(!is.na(lower_50)) %>% droplevels() %>% unique()
  return(df_sum)
}

# Smooth bands with monthly midpoints
loess_bands <- function(df, column, band){
  df2 <- df[ , c(column, "mon_num")]
  colnames(df2) <- c("y", "mon_num")
  loess_mod <- loess(y ~ mon_num, span = 0.6, data = df2)
  mid_pts <- data.frame(mon_num = c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10))
  loess_pred <- data.frame(smooth = predict(loess_mod, newdata = mid_pts))
  colnames(loess_pred) <- paste0("smooth_", band)
  return(loess_pred)
}

site <- "NETN_MABI_SA00"
park <- substr(site, 6, 9)

remove <- c("DOsat_pct", "Turbidity_NTU", "SDepth1_m", "PenetrationRatio") #chars to remove from list

char_list <- getCharInfo(netnwd, 
                         park = park, 
                         sitecode = site, 
                         category = "physical", 
                         info = "CharName") %>% 
  .[! . %in% remove] #remove chars above from final list
char_list_ord = c("DO_mgL", "Temp_C", "SpCond_uScm", "Discharge_cfs", "pH")
char_list
char_list_ord

#----- Plot function testing -----
water_plot <- function(site, char){
  
  park = substr(site, 6, 9)
  sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")
  
  # Set unit for plot display
  unit <- getCharInfo(netnwd, park = park, sitecode = site, charname = char, info = "Units") %>% 
    ifelse(. == "pct", paste("%"), .) %>% 
    ifelse(. == "pH units", paste(""), .)
  
  # Compile historic water data
  water_dat_hist <- getWData(netnwd, park = park, sitecode = site, 
                             charname = char, years = 2006:2018) %>% 
    mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) 
  
  # Compile target year water data
  water_dat_new <- getWData(netnwd, park = park, sitecode = site,
                            charname = char, years = 2019) %>% 
    mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
           mon_num = as.numeric(month))
  water_dat_new$year <- format(as.Date(water_dat_new$Date), "%Y")
  
  # Historic percentile band values
  water_pct <- pct_fun(water_dat_hist) 
  
  final_data <- merge(water_dat_new, water_pct, 
                      by = c("mon_num", "Park", "month", "Site", "Characteristic"), 
                       all.x = T, all.y = T)
  
  # Create y axis label with units in parentheses, unless it's pH (no units)
  ylabel <- getCharInfo(netnwd, parkcode = park, sitecode = site, charname = char,
                        info = "DisplayName") %>% 
    ifelse(. != "pH", paste0(.," (", unit, ")"), .)
  
  # Create label for point data by removing units from char 
  # (this still needs to be improved)
  ptlabel <- gsub("_.*","",char)
  
  monthly_plot <- 
    ggplot(data = final_data, aes(x = mon_num, y = median_val))+
    geom_ribbon(aes(x = mon_num, ymax = upper_100, ymin = lower_100), #, text = "Historic range"), 
                fill = "#c3d2ee", alpha = 0.8)+
    geom_ribbon(aes(x = mon_num, ymax = upper_95, ymin = lower_95), #, text = "Historic 95% range"), 
                fill = "#84a1e1", alpha = 0.8)+
    geom_ribbon(aes(x = mon_num, ymax = upper_50, ymin = lower_50), #, text = "Historic 50% range"), 
                fill = "#5979c0", alpha = 0.8)+
    #stat_smooth(method = "loess", aes(x = mon_num, y = median_val, text = "Historic median"), 
                #color = "#1A52D0", position = "identity", se = F, formula = y ~ x, span = 0.6)+
    geom_line(color = "#064be0")+
    labs(y = ylabel, x = NULL, title = NULL) +  
    geom_point(aes(x = mon_num, y = ValueCen))+ #, text = paste0(month, " ", year, "<br>", 
                                             #                ptlabel, ": ", round(ValueCen,1), " ", unit))) +
    forestMIDN::theme_FVM()+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10), 
                       labels = c("5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", 
                                  "9" = "Sep", "10" = "Oct")) #update for ACAD
  
  #plot <- ggplotly(monthly_plot, tooltip = c("text"))
  
  return(monthly_plot)
}


#water_plot("NETN_MABI_SA00", char_list[1])

# Create list of ggplots and rename them
plot <- purrr::map(char_list, ~water_plot(site = "NETN_MABI_SA00", char = .)) %>%
        set_names(c(char_list)) #name plots to refer to them by char
        #purrr::map(., ~ggplotly(., tooltip = c("text"))) #plotly iteration

sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")

plots2 <- purrr::map(char_list_ord, ~water_plot(site = "NETN_MABI_SA00", char = .)) %>%
          set_names(c(char_list_ord)) %>% 
          gridExtra::grid.arrange(grobs = ., ncol = 2,
                                  top = paste("Current vs. Historic Conditions for: ", sitename))

plots2 <- purrr::map(char_list_ord, ~water_plot(site = "NETN_MABI_SA00", char = .)) %>%
  set_names(c(char_list_ord)) %>% 
  gridExtra::grid.arrange(grobs = ., ncol = 2,
                          top = paste("Current vs. Historic Conditions for: ", sitename))
# "Historic median =  "#064be0"

leg_df <- data.frame(metric = c("Historic min/max", 
                               "Historic 95% range",
                               "Historic 50% range",
                               "Current value"),
                     x = 1:4, 
                     y = 1:4
                     )
leg_df

leg_plot <- ggplot(leg_df[1:3,], aes(x = x, y = y, fill = metric, color = metric,
                                                               shape = metric, group = metric))+
            geom_tile()+
            geom_point(data = leg_df[4,])+
            scale_fill_manual(values = c("#c3d2ee", 
                                         "#84a1e1", 
                                         "#5979c0", 
                                         "#FFFFFF"),
                              breaks = c("Historic min/max", 
                                         "Historic 95% range",
                                         "Historic 50% range",
                                         "Current value"),
                              labels = c("Historic min/max", 
                                         "Historic 95% range",
                                         "Historic 50% range",
                                         "Current value"), 
                              name = NULL)+
           scale_shape_manual(values = c(NA, NA, NA, 16))+
           scale_color_manual(values = c(NA, NA, NA, "white"))


leg_bands

leg_point <- ggplot(leg_df %>% filter(x==4), aes(x = x, y = y, color = metric))+
                    geom_point(shape = 16)+
                    scale_color_manual(values = "black", labels = "Current Value", name = NULL)+
                    theme(legend.position= 'right',
                          legend.key = element_blank())

leg_point

?scale_shape_manual
leg <- cowplot::get_legend(leg_plot)

plots2_leg <- plots2 + annotation_custom(ggplotGrob(leg))


names(plot)
length(plot)
char_list
plot["DO_mgL"]  
plot[2]
plot
