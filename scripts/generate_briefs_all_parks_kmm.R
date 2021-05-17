#------------------
# Generate briefs for all LNETN parks
# Code iterates through params$park and renames html output with park code
#------------------
library(purrr)
library(knitr)
library(rmarkdown)

lnetn_parks <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
indir <- paste0("./rmd/")
outdir <- paste0("./rmd/output/latest/")
rmd <- c("LNETN_automated_summary_20210219.Rmd")

render_reports <- function(park_list){
                  render(input = paste0(indir, rmd),
                    params = list(park = park_list, nutrient = TRUE),
                    output_file = paste0("NETN_WQ_summary_", park_list, "_20210420.html"),
                    output_dir = outdir)
  
}

render_reports("ACAD")

map(lnetn_parks, ~render_reports(.))
 
