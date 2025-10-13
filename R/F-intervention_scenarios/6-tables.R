# Setup -----------------------------------------------------------------------
library(dplyr)
library(tidyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/labels.R", local = TRUE)

d_raw <- readr::read_csv("data/output/table_test.csv")

# Tables and Plots:
#
# T1: scenarios and params
# T2: baseline, +30%: OTC 0, 25, 50, 75 100 - PIA, GFR, FLARES, 2 resists
#   - impact of OTC cov
# P1: OTC VS cli dynamics
# T3: +30%: OTC 30, vary disc, gfr test, some HIVtest - PIA, GFR, FLARES, 2 resists
#   - mitigation
# P2: PIA | Resist ~ prop_otc * test_rate

t2_scenarios <- c(
  "baseline",
  "otc_best_some_hivtst_50",
  "plot_cli1.52_otc0_tst0.5", # 0% OTC
  "plot_cli1.1_otc0.2_tst0.5", # 25% OTC
  "plot_cli0.7_otc0.42_tst0.5", # 50% OTC
  "plot_cli0.3_otc0.65_tst0.5", # 77% OTC
  "plot_cli0_otc0.85_tst0.5" # 100% OTC
)


t2_cols <- c(
  "lst_prep_any",
  "lst_prop_otc",
  "cml_pia_all",
  "lst_prep_any_gfr_lt60",
  "cml_hbv_flare_otc_ir100kpy",
  "cml_resist_tdf_100i",
  "cml_resist_ftc_100i"
  # "cml_addi_resist_tdf_nia",
  # "cml_addi_resist_ftc_nia"
)
t2_cols <- var_labels[t2_cols]

t2 <- tibble(scenario_name = t2_scenarios) |>
  left_join(d_raw) |>
  select(scenario_name, any_of(t2_cols))
names(t2) <- var_labels[names(t2)]
write.csv(t2, "data/output/pres_t2.csv", row.names = FALSE)

t2 |> DT::datatable()

# use best as ref here?
# or baseline?
t3_scenarios <- c(
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
  "otc_best_some_hivtst_100"
)

t3_cols <- c(
  "lst_prep_any",
  "lst_prop_otc",
  "cml_pia_all",
  "lst_prep_any_gfr_lt60",
  "lst_prep_otc_gfr_lt60",
  "cml_hbv_flare_otc_ir100kpy",
  "cml_resist_tdf_100i",
  "cml_resist_ftc_100i"
  # "cml_addi_resist_tdf_nia",
  # "cml_addi_resist_ftc_nia"
)
t3_cols <- var_labels[t3_cols]

t3 <- tibble(scenario_name = t3_scenarios) |>
  left_join(d_raw) |>
  select(scenario_name, any_of(t3_cols))
t3$scenario_name <- nicefy_scs_names(t3$scenario_name)
names(t3) <- var_labels[names(t3)]
write.csv(t3, "data/output/pres_t3.csv", row.names = FALSE)

t3 |> DT::datatable()
