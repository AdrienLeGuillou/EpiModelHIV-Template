# Setup -----------------------------------------------------------------------
library(dplyr)
library(tidyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/labels.R", local = TRUE)

d_raw <- readr::read_csv("data/output/table_test.csv")
d_raw <- readr::read_csv("data/output/table.csv")

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
  "no_otc_prep_or152",
  "add_otc_best0.03027552",
  "add_otc_best0.06595926",
  "add_otc_best0.1043088",
  "add_otc_best0.14680892",
  "add_otc_best0.19494439",
  "add_otc_best0.25019997",
  "add_otc_best0.31406043",
  "add_otc_best0.38801056",
  "add_otc_best0.47353511",
  "add_otc_best0.57211886",
  "add_otc_indics0.0378067",
  "add_otc_indics0.08089587",
  "add_otc_indics0.12802123",
  "add_otc_indics0.18089827",
  "add_otc_indics0.24124246",
  "add_otc_indics0.31076927",
  "add_otc_indics0.39119419",
  "add_otc_indics0.48423269",
  "add_otc_indics0.59160025",
  "add_otc_indics0.71501234"
  # "plot_cli1.52_otc0_tst0.5", # 0% OTC
  # "plot_cli1.1_otc0.2_tst0.5", # 25% OTC
  # "plot_cli0.7_otc0.42_tst0.5", # 50% OTC
  # "plot_cli0.3_otc0.65_tst0.5", # 77% OTC
  # "plot_cli0_otc0.85_tst0.5" # 100% OTC
)


t2_cols <- c(
  "lst_prep_any",
  "lst_prop_otc",
  "cml_pia_all",
  "lst_prep_any_gfr_lt60",
  # "cml_hbv_flare_otc_ir100kpy",
  "cml_hbv_flare_otc",
  "cml_resist_tdf_100i",
  "cml_resist_ftc_100i"
  # "cml_addi_resist_tdf_nia",
  # "cml_addi_resist_ftc_nia"
)
t2_cols <- var_labels[t2_cols]

t2 <- tibble(scenario_name = t2_scenarios) |>
  left_join(d_raw) |>
  select(scenario_name, any_of(t2_cols))
t2$scenario_name <- nicefy_scs_names(t2$scenario_name)
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
  "otc_best_some_hivtst_100",

  "otc_indics_some_hivtst_50",
  "otc_indics_disc_050",
  "otc_indics_disc_075",
  "otc_indics_disc_125",
  "otc_indics_disc_150",
  "otc_indics_gfr_5",
  "otc_indics_gfr_3",
  "otc_indics_gfr_1",
  "otc_indics_gfr_same",
  "otc_indics_some_hivtst_0",
  "otc_indics_some_hivtst_25",
  "otc_indics_some_hivtst_50",
  "otc_indics_some_hivtst_75",
  "otc_indics_some_hivtst_100"
)

t3_cols <- c(
  "lst_prep_any",
  "lst_prop_otc",
  "cml_pia_all",
  "lst_prep_any_gfr_lt60",
  "lst_prep_otc_gfr_lt60",
  "cml_hbv_flare_otc_ir100kpy",
  # "cml_hbv_flare_otc",
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