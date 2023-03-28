#------------------
# Generate briefs for all parks
# Code iterates through params$park and renames html output with park code
#------------------
library(purrr)
library(knitr)
library(rmarkdown)

indir <- c("./rmd/")
outdir <- c("./rmd/output/")

#----- Generate water data files- must do each time new data are to be incorporated
# library(NCRNWater)
# 
# compileNETNdata(path = "./Data",
#                 export = TRUE, surface = TRUE, active_site = TRUE,
#                 active_metric = TRUE, restricted = TRUE)

#----- Reports for Lower NETN
lnetn_parks <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
render_LNETN_reports <- function(park_list, nutrient, plottype = 'bands'){
                          render(input = "./rmd/LNETN_water_summary_2023.Rmd",
                          params = list(park = park_list, from = 2006, current = 2022, 
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

#----- Report for ACAD
render_ACAD_report <- function(park_list, year, nutrient){
  render(input = paste0(indir, acad_rmd),
         params = list(park = park_list, nutrient = nutrient, 
                       from = 2006, to = year - 1, current = year),
         output_file = paste0("NETN_water_summary_", 
                              park_list, "_", #"_2021", 
                              format(Sys.time(), '%b_%Y'),
                              ".html"),
         output_dir = outdir,
         output_options = list(self_contained = TRUE))
}
  
render_ACAD_report("ACAD", 2022, nutrient = TRUE)


