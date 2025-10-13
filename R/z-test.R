# Scratchpad for interactive testing before integration in a script

# rmarkdown::render(
#   "R/Z-calibration/calibration_values.Rmd",
#   output_file = "calibration_report.html",
#   knit_root_dir = getwd(),
#   output_dir = "./"
# )

rmarkdown::render(
  "./Rmd/scenarios_explore.Rmd",
  output_file = "scenarios_explore.html",
  knit_root_dir = getwd(),
  output_dir = "./"
)

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_light())

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/outcomes.R", local = TRUE)

scenarios_tibble_dir <- fs::path(scenarios_dir, "merged_tibbles")
d_ref <- make_d_ref(fs::path(scenarios_tibble_dir, "df__baseline.rds"))

b_infos <- EpiModelHPC::get_scenarios_tibble_infos(scenarios_tibble_dir) |>
  filter(
    stringr::str_detect(scenario_name, "cli[0-9\\.]*_otc[0-9\\.]*_tst[0-9\\.]*")
  ) |>
  mutate(scenario_name_tmp = scenario_name) |>
  separate_wider_regex(
    scenario_name_tmp,
    c("cli[0-9\\.]*_otc[0-9\\.]*_tst", test_p = "[0-9\\.]*")
  )

d_cont <- lapply(seq_len(nrow(b_infos)), \(i) {
  process_one_scenario(b_infos[i, ], d_ref) |>
    mutate(tst_rate = as.numeric(b_infos[i, "test_p"])) |>
    select(
      tst_rate,
      lst_prop_otc,
      cml_pia_all,
      cml_resist,
      cml_addi_resist_nia
    ) |>
    summarise(across(everything(), median))
}) |>
  bind_rows()

# ------------------------------------------------------------------------------

library(dplyr)
source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/outcomes.R", local = TRUE)

d_sc_raw <- readRDS(fs::path(output_dir, paste0("d_raw.rds")))

d_sc_raw <- d_sc_raw |>
  mutate(
    cml_resist_tdf_100i = cml_resist_tdf / cml_incid * 100,
    cml_resist_ftc_100i = cml_resist_ftc / cml_incid * 100,
  )


source("R/F-intervention_scenarios/labels.R", local = TRUE)

format_table(d_sc_raw, var_labels, format_patterns) |>
  write.csv(fs::path(output_dir, "table_test.csv"), row.names = FALSE)
