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

sim <- readRDS("./data/run/scenarios/sim__only_otc_same_1__1.rds")
sim <- readRDS("./data/run/scenarios/sim__only_otc_free_1__1.rds")
d <- as_tibble(sim$run$sim1$attr)

d |>
  filter(
    prep.otc == 1,
    gfr < 90
  )

d <- as_tibble(sim)
d$dbg_prep_otc_gfr_stop |> tail(50)
