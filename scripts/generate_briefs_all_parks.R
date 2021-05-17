#------------------
# Generate briefs for all LNETN parks
# Code iterates through params$park and renames html output with park code
#------------------
library(purrr)
library(tidyverse)

lnetn_parks <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
indir <- paste0("./rmd/")
outdir <- paste0("./rmd/output/")
rmd <- c("NETN_automated_summary_physical_params_20201124.Rmd")

render_reports <- function(park_list){
  rmarkdown::render(input = paste0(indir, rmd),
                    params = list(park = park_list),
                    output_file = paste0("Lower_NETN_WQ_physical_parameters_", park_list, ".html"),
                    output_dir = outdir)
  
}

map(lnetn_parks, ~render_reports(.))
