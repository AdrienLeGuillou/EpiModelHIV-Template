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

est <- readRDS("./data/run/estimates/restart-hpc.rds")
est$run$sim1$attr$syph.inf|> mean()
