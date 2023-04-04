#------------------
# Generate briefs for all parks
# Code iterates through params$park and renames html output with park code
#------------------
library(purrr)
library(knitr)
library(rmarkdown)


#----- Generate water data files- must do each time new data are to be incorporated
# library(NCRNWater)
# 
# compileNETNdata(path = "./Data",
#                 export = TRUE, surface = TRUE, active_site = TRUE,
#                 active_metric = TRUE, restricted = TRUE)

#----- Reports for Lower NETN with bands
lnetn_parks <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
render_LNETN_reports <- function(park_list, year = 2022, nutrient, plottype = 'bands'){
                          render(input = "./rmd/LNETN_water_summary_2023.Rmd",
                          params = list(park = park_list, from = 2006, current = year, 
                                        nutrient = nutrient, plottype = plottype),
                          output_file = paste0("NETN_water_summary_", 
                            park_list, "_", format(Sys.time(), '%b_%Y'), ".html"),
                          output_dir = "./rmd/output/",
                          output_options = list(self_contained = TRUE))#,
                          #encoding = "UTF-8")
  
}

map(lnetn_parks[1:6], ~render_LNETN_reports(., nutrient = TRUE))
map(lnetn_parks[7], ~render_LNETN_reports(., nutrient = FALSE))

render_LNETN_reports("SAIR", nutrient = FALSE, plottype = 'boxplot')

render_LNETN_reports_box <- function(park_list, year = 2022, 
                                     nutrient = TRUE, 
                                     plottype = 'boxplot'){
  render(input = "./rmd/LNETN_water_summary_2023.Rmd",
         params = list(park = park_list, from = 2006, current = year, 
                       nutrient = nutrient, plottype = plottype),
         output_file = paste0("NETN_water_summary_", 
                              park_list, "_", format(Sys.time(), '%b_%Y'), 
                              "_boxplots.html"),
         output_dir = "./rmd/output/",
         output_options = list(self_contained = TRUE))}

map(lnetn_parks[1], ~render_LNETN_reports_box(., nutrient = TRUE))

#----- Report for ACAD
render_ACAD_report <- function(year, nutrient, plottype = 'bands'){
  render(input = paste0(input = "./rmd/ACAD_water_summary_2023.Rmd"),
         params = list(from = 2006, current = year, 
                       nutrient = nutrient, plottype = plottype),
         output_file = paste0("NETN_water_summary_ACAD_",
                              format(Sys.time(), '%b_%Y'), ".html"),
         output_dir = "./rmd/output/",
         output_options = list(self_contained = TRUE))
}

render_ACAD_report(year = 2022, nutrient = TRUE, plottype = 'bands')


