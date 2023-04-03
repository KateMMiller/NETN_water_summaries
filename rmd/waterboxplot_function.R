#-------------------------------------------------
# Function to generate boxplot figures in plotly, only if params$plottype != 'bands'
#-------------------------------------------------
# parkcode = "MIMA"; sitecode = "NETN_MIMA_SA00"; chars = c("ANC_ueqL", "TN_mgL", "TP_ugL")
# data = wdata_full
# charname = chars[2]
# 
# waterboxplot(data = wdata_full, parkcode = "ACAD",
#              sitecode = "NETN_MIMA_SA00", charname = chars[3])

waterboxplot <- function(data = wdata_full, parkcode, sitecode, charname) {
  
  # params for package function
  category <- "nutrients"
  assessment <- TRUE # function param for assessment line
  
  # Select target months using site type
  # Note: this may have problems later if target months are different
  wdat <- data |> filter(Site == sitecode, Characteristic == charname)
  
  if (all(wdat$SiteType == "Lake")) {
    wdat <- filter(wdat, month == "Jun" | month == "Aug")
  } else {
    wdat <- filter(wdat, month == "May" | month == "Aug")}
  
  yname = unique(wdat$yname)
  param_name = unique(wdat$param_name)
  unit = unique(wdat$unit)
  
  # Set y axis style for plotly
  yaxis = list(
    zeroline = FALSE, #turn off zero line
    title = yname,
    showline = TRUE,
    showgrid = FALSE,
    autotick = TRUE,
    ticks = "outside",
    range = c(range(wdat$ValueCen))
  )
  
  # Set x axis style for plotly
  xaxis = list(
    title = FALSE, 
    showline = TRUE,
    showgrid = FALSE,
    autotick = FALSE,
    ticks = "outside",
    #ticktext = list("Jun", "Aug"),
    ticktext = sort(unique(wdat$month)),
    #tickvals = list(6, 8),
    tickvals = sort(unique(wdat$month_num)),
    tickmode = "array"
  )
  
  # Create marker tooltip for hovertemplate
  hover = paste0('%{x} ', current, '<br>', # Month Year
                 param_name, ': %{y} ', unit, # Parameter value units
                 '<extra></extra>') # this removes the secondary text box in the tooltip
  
  wdat_hist <- wdat |> filter(year < current)
  wdat_curr <- wdat |> filter(year == current)

  # Ensures orange symbol shows in legend
  oranged <- 
    if(nrow(wdat_curr[wdat_curr$pcolor == "Poor WQ value", ]) == 0){
      data.frame(month_num = 5, ValueCen = -999)
    } else {
      wdat_curr[wdat_curr$pcolor == "Poor WQ value", ]}
  
    p <- plot_ly(wdat_hist, x = ~month_num, y = ~ValueCen) |> 
    
    # Boxplots historic range
    add_boxplot(boxpoints = "outliers", name = "Historic range", 
                marker = list(symbol='asterisk-open', size = 7, color = "#1378b5"),
                fillcolor = list(color = "#1378b5", alpha = 0.85), #showlegend = legend, 
                line = list(color = "#1378b5")) |> #, showlegend = FALSE) |>  
    
    # Current year measurements
    # Use pcolor to set color and name of markers
    add_markers(data = wdat_curr[wdat_curr$pcolor=="Current value",], 
                name = "Current value",
                marker = list(color = "black", size = 7),
                hovertemplate = hover) |> 
    
    # Orange markers; rbind -999 makes markers show up if no poor WQ in that plot
    add_markers(data = oranged,
                name = "Poor WQ value",
                marker = list(color = "orange", size = 7),
                hovertemplate = hover) |> 
    
    # Set x axis and y axis styles
    layout(xaxis = xaxis, yaxis = yaxis, 
           # make legend horizontal
           legend = list(orientation = "h"))
  
  # Set value for WQ threshold line 
  UpperPoint <- unique(wdat_curr$UpperPoint)
  
  # Find min and max months
  wq_x <- min(unique(wdat$month_num))
  wq_xend <- max(unique(wdat$month_num))
  
  # Calculate length of WQ line based on plotted months
  wq_xend <- ifelse(wq_x == 6, wq_xend+1,
                    ifelse(wq_x == 5, wq_xend+1.5,
                           NA))
  wq_x <- ifelse(wq_x == 6, wq_x-1,
                 ifelse(wq_x == 5, wq_x-1.5,
                        NA))
  
  # If there is an upper threshold value, add WQ line to plot
  UpperPoint = ifelse(is.na(UpperPoint), 0, UpperPoint)
#  ifelse(!is.na(UpperPoint), 
  p <- p |> add_segments(y = UpperPoint, yend = UpperPoint,
                         x = wq_x, xend = wq_xend, # length of line
                         text = paste("Upper", param_name, "threshold:", UpperPoint, unit),
                         hoverinfo = "text", # set tooltip text
                         line = list(color = "black", dash = "dash"),
                         name = "WQ threshold") |> 
            layout(showlegend = FALSE)
  return(p)
}
