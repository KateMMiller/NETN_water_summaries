#------------------
# Generate briefs for all parks
# Code iterates through params$park and renames html output with park code
#------------------
library(purrr)
library(knitr)
library(rmarkdown)

lnetn_parks <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
indir <- paste0("./rmd/")
outdir <- paste0("./rmd/output/")
rmd <- c("LNETN_water_summary.Rmd")

render_LNETN_reports <- function(park_list){
                          render(input = paste0(indir, rmd),
                          params = list(park = park_list, nutrient = TRUE),
                          output_file = paste0("NETN_water_summary_", 
                            park_list, "_", format(Sys.time(), '%b_%Y'),".html"),
                          output_dir = outdir)
  
}

map(lnetn_parks, ~render_LNETN_reports(.))

render_ACAD_report <- function(park_list){
  render(input = paste0(indir, rmd),
         params = list(park = park_list, nutrient = TRUE),
         output_file = paste0("NETN_water_summary_", 
                              park_list, "_", format(Sys.time(), '%b_%Y'),".html"),
         output_dir = outdir)
}
  
render_ACAD_report("ACAD")


 
