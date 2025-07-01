# Scratchpad for interactive testing before integration in a script

# rmarkdown::render(
#   "R/Z-calibration/calibration_values.Rmd",
#   output_file = "calibration_report.html",
#   knit_root_dir = getwd(),
#   output_dir = "./"
# )

source("R/shared_variables.R", local = TRUE)

library(dplyr)
library(tidyr)

d <- readRDS("./data/run/scenarios/merged_tibbles/df__no_otc.rds")
d <- readRDS("./data/run/scenarios/merged_tibbles/df__otc_free_1.rds")

glimpse(d)
