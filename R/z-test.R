# Scratchpad for interactive testing before integration in a script

# rmarkdown::render(
#   "R/Z-calibration/calibration_values.Rmd",
#   output_file = "calibration_report.html",
#   knit_root_dir = getwd(),
#   output_dir = "./"
# )

# rmarkdown::render(
#   "./Rmd/scenarios_explore.Rmd",
#   output_file = "scenarios_explore.html",
#   knit_root_dir = getwd(),
#   output_dir = "./"
# )

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_light())

# source("R/shared_variables.R", local = TRUE)
# source("R/F-intervention_scenarios/outcomes.R", local = TRUE)
# scenarios_tibble_dir <- "./data/run/indics_raw/merged_tibbles/"
# scenarios_info <- EpiModelHPC::get_scenarios_tibble_infos(scenarios_tibble_dir)
# d_ref <- make_d_ref(fs::path(scenarios_tibble_dir, "df__baseline.rds"))
# d_ls <- lapply(
#   seq_len(nrow(scenarios_info)),
#   \(i) process_one_scenario(scenarios_info[i, ], d_ref)
# )
# d_sc_raw <- dplyr::bind_rows(d_ls)
# saveRDS(d_sc_raw, "./data/run/indics_raw/d_raw.rds")

d_sc_raw <- readRDS("./data/run/indics_raw/d_raw.rds")

d_sc_raw <- d_sc_raw |>
  mutate(
    otc_or = as.numeric(stringr::str_extract(scenario_name, "[0-9\\.]+")),
    grp = stringr::str_extract(scenario_name, "(best)|(indic)")
  ) |>
  filter(scenario_name != "baseline")
d_sc_raw$otc_or

d_best <- d_sc_raw |> filter(grp == "best")
d_indic <- d_sc_raw |> filter(grp == "indic")

d <- d_indic
mod_otc <- lm(otc_or ~ poly(lst_prop_otc, 3), data = d)
summary(mod_otc)
d_pred <- tibble(lst_prop_otc = seq(0.05, 0.5, 0.01))
d_pred$otc_or <- predict(mod_otc, newdata = d_pred)

ggplot(d, aes(y = otc_or, x = lst_prop_otc)) +
  geom_point() +
  geom_line(data = d_pred, aes(y = otc_or, x = lst_prop_otc))

d_interest <- tibble(lst_prop_otc = seq(0.05, 0.5, 0.05))
d_interest$otc_or <- predict(mod_otc, newdata = d_interest)
