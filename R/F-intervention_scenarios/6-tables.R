# Setup -----------------------------------------------------------------------
library(dplyr)
library(tidyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/labels.R", local = TRUE)

d_raw <- readr::read_csv("data/output/table.csv")

t2_scenarios <- c(
  "baseline",
  "no_otc_prep_or150",
  "otc_best_some_hivtst_50",
  "otc_best_disc_050",
  "otc_best_disc_075",
  "otc_best_disc_125",
  "otc_best_disc_150",
  "otc_best_gfr_5",
  "otc_best_gfr_3",
  "otc_best_gfr_1",
  "otc_best_gfr_same",
  "otc_best_some_hivtst_0",
  "otc_best_some_hivtst_25",
  "otc_best_some_hivtst_50",
  "otc_best_some_hivtst_75",
  "otc_best_some_hivtst_100",
  "plot_cli1.52_otc0_tst50", # 0% OTC
  "plot_cli1.1_otc0.2_tst50", # 25% OTC
  "plot_cli0.7_otc0.42_tst50", # 50% OTC
  "plot_cli0.3_otc0.65_tst50", # 77% OTC
  "plot_cli0_otc0.85_tst50" # 100% OTC
)

t2_cols <- c(
  "cml_pia_all",
  "lst_prep_any_gfr_lt60",
  "cml_hbv_flare_otc_ir100kpy",
  "cml_addi_resist_tdf_nia",
  "cml_addi_resist_ftc_nia"
)
t2_cols <- var_labels[t2_cols]

d_raw |>
  semi_join(tibble(scenario_name = t2_scenarios)) |>
  # filter(scenario_name %in% t2_scenarios) |>
  select(scenario_name, any_of(t2_cols))
