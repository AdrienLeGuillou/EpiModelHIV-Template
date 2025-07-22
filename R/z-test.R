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
library(ggplot2)

theme_set(theme_light())

d <- readRDS("./data/run/scenarios/merged_tibbles/df__otc_best_guess.rds")

d |>
  select(sim, time, prepCurr, prep.otcCurr) |>
  mutate(prep_any = prepCurr + prep.otcCurr) |>
  pivot_longer(-c(sim, time)) |>
  ggplot(aes(x = time / 52, y = value, col = name)) +
  geom_smooth()

rmarkdown::render(
  "./Rmd/scenarios_explore.Rmd",
  output_file = "scenarios_explore.html",
  knit_root_dir = getwd(),
  output_dir = "./"
)
