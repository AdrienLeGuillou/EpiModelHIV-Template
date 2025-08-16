# Setup ------------------------------------------------------------------------
library(dplyr)
library(tidyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/labels.R", local = TRUE)

first_table_labels <- c(
  # Process
  "lst_prep_any",
  "lst_prep_otc_num",
  # Epi
  # "lst_ir100",
  # "cml_incid",
  # "cml_nia_all",
  "cml_pia_all",
  # "cml_nnt_otc",
  "lst_resist_hiv_prev",
  # "cml_gfr_drop",
  # "cml_gfr_drop_gfr90",
  # "cml_gfr_drop_yo50",
  "lst_prep_any_gfr_lt60",
  # "cml_hbv_flare",
  "lst_hbv_flare_ir100k"
)

labels <- var_labels[first_table_labels]

first_table_scs <- tibble(
  scenario_name = c(
    "baseline",
    "base_only_otc_same_adhr_base",
    "only_otc_best_adhr_base",
    "only_otc_relaxed_adhr_base",
    "no_otc_prep_or150",
    "otc_best_adhr_base",
    "otc_best_mix_adhr_base"
  )
)

ft <- readr::read_csv("data/output/table.csv")

ft <- first_table_scs |>
  left_join(ft, by = "scenario_name") |>
  select(scenario_name, any_of(unname(labels))) |>
  mutate(scenario_name = nicefy_scs_names(scenario_name))

readr::write_csv(ft, "data/output/table1.csv")
