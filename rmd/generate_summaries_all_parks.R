#------------------
# Generate briefs for all parks
# Code iterates through params$park and renames html output with park code
#------------------
library(purrr)
library(knitr)
library(rmarkdown)

indir <- paste0("./rmd/")
outdir <- paste0("./rmd/output/")
rmd <- c("LNETN_water_summary.Rmd")
acad_rmd <- c("ACAD_water_summary.Rmd")

#----- Generate water data files
library(NCRNWater)
compileNETNdata(path = "./data", 
                export = TRUE, surface = TRUE, active_site = TRUE, 
                active_metric = TRUE, restricted = TRUE)

#----- Reports for Lower NETN
lnetn_parks <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
render_LNETN_reports <- function(park_list){
                          render(input = paste0(indir, rmd),
                          params = list(park = park_list, nutrient = TRUE),
                          output_file = paste0("NETN_water_summary_", 
                            park_list, "_2019", 
                            #format(Sys.time(), '%b_%Y'),
                            ".html"),
                          output_dir = outdir)
  
}

map(lnetn_parks, ~render_LNETN_reports(.))

render_LNETN_reports("SAIR")

#----- Report for ACAD
render_ACAD_report <- function(park_list, year, nutrient){
  render(input = paste0(indir, acad_rmd),
         params = list(park = park_list, nutrient = nutrient, from = 2006, to = year - 1, current = year),
         output_file = paste0("NETN_water_summary_", 
                              park_list, "_", #"_2021", 
                              format(Sys.time(), '%b_%Y'),
                              ".html"),
         output_dir = outdir)
}
  
render_ACAD_report("ACAD", 2021, nutrient = FALSE)


 
