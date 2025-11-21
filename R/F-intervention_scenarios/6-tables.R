# Setup -----------------------------------------------------------------------
library(dplyr)
library(tidyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/labels.R", local = TRUE)

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
  "add_otc_samex1.30",
  "add_otc_sdurx1.10",
  "add_otc_sdurx1.20",
  "add_otc_sdurx1.30",
  "add_otc_sdurx1.40",
  "add_otc_sdurx1.50"
)


t2_cols <- c(
  # "lst_prep_any",
  # "lst_prop_otc",
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

# t2 |> DT::datatable()

# use best as ref here?
# or baseline?
t3_scenarios <- c(
  "otc_sdur_some_hivtst_50",
  "otc_sdur_disc_050",
  "otc_sdur_disc_075",
  "otc_sdur_disc_125",
  "otc_sdur_disc_150",
  "otc_sdur_gfr_Inf",
  "otc_sdur_gfr_5",
  "otc_sdur_gfr_3",
  "otc_sdur_gfr_1",
  "otc_sdur_gfr_same",
  "otc_sdur_some_hivtst_0",
  "otc_sdur_some_hivtst_25",
  "otc_sdur_some_hivtst_50",
  "otc_sdur_some_hivtst_75",
  "otc_sdur_some_hivtst_100",
  "otc_sdur_hivtst_13",
  "otc_sdur_hivtst_26",
  "otc_sdur_hivtst_52",
  "otc_sdur_hivtst_104",
  "otc_sdur_hivtst_208",
  "otc_sdur_stitst_13",
  "otc_sdur_stitst_26",
  "otc_sdur_stitst_52",
  "otc_sdur_stitst_104",
  "otc_sdur_stitst_208"
)

t3_cols <- c(
  "lst_additional_prep",
  "cml_pia_all",
  "lst_prep_any_gfr_lt60",
  "lst_prep_otc_gfr_lt60",
  "cml_hbv_flare_otc",
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

# t3 |> DT::datatable()