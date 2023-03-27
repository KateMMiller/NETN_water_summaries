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
library(NCRNWater)

compileNETNdata(path = "./Data",
                export = TRUE, surface = TRUE, active_site = TRUE,
                active_metric = TRUE, restricted = TRUE)

#----- Reports for Lower NETN
lnetn_parks <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
render_LNETN_reports <- function(park_list, nutrient){
                          render(input = "./rmd/LNETN_water_summary_2023.Rmd",
                          params = list(park = park_list, from = 2006, current = 2022, 
                                        nutrient = nutrient, plottype = 'bands'),
                          output_file = paste0("NETN_water_summary_", 
                            park_list, "_", format(Sys.time(), '%b_%Y'), ".html"),
                          output_dir = "./rmd/output/")
  
}

map(lnetn_parks, ~render_LNETN_reports(., nutrient = TRUE))
render_LNETN_reports("SAIR", nutrient = FALSE, plottype = 'boxplot')



render_LNETN_reports_2019 <- function(park_list){
  render(input = paste0(indir, rmd),
         params = list(park = park_list, to = 2018, current = 2019, nutrient = TRUE),
         output_file = paste0("NETN_water_summary_", 
                              park_list, "_", "2019", 
                              ".html"),
         output_dir = outdir)
  
}

map(lnetn_parks, ~render_LNETN_reports_2019(.))
map(c("MABI", "MIMA", "SAGA", "SAIR"), ~render_LNETN_reports(.))

render_LNETN_reports("SAIR", nutrient = TRUE)



#----- Report for ACAD
render_ACAD_report <- function(park_list, year, nutrient){
  render(input = paste0(indir, acad_rmd),
         params = list(park = park_list, nutrient = nutrient, 
                       from = 2006, to = year - 1, current = year),
         output_file = paste0("NETN_water_summary_", 
                              park_list, "_", #"_2021", 
                              format(Sys.time(), '%b_%Y'),
                              ".html"),
         output_dir = outdir)
}
  
render_ACAD_report("ACAD", 2021, nutrient = FALSE)

render_ACAD_report_2020 <- function(park_list, year, nutrient){
  render(input = paste0(indir, acad_rmd),
         params = list(park = park_list, nutrient = nutrient, from = 2006, to = year - 1, current = year),
         output_file = paste0("NETN_water_summary_", 
                              park_list, "_", "2020", 
#                              format(Sys.time(), '%b_%Y'),
                              ".html"),
         output_dir = outdir)
}

render_ACAD_report_2020("ACAD", 2021, nutrient = FALSE)

 
