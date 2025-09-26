## 0. Generate the scenarios.csv file
##
## Programatically define some scenarios and store them to
## "data/input/scenarios.csv"

# Restart R before running this script (Ctrl_Shift_F10 / Cmd_Shift_0)

# Setup ------------------------------------------------------------------------
library(EpiModelHIV)
library(dplyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/utils-scenarios.R", local = TRUE)


# Scenarios DF list ------------------------------------------------------------
sc_df_ls <- list()
sc_names <- c()

# No OTC +30% ORs: (CLI: 1, OTC: 0)
#   - CLI: 1.5 (maybe 1.52)
#   - OTC: 0
# OTC best ORs: (CLI: 0.7, OTC: 0.3)
#   - CLI: 1
#   - OTC: 0.25
# OTC mix ORs: (CLI: 0.38, OTC: 0.62)
#   - CLI: 0.52
#   - OTC: 0.52

# I have a relationship of the type:
# Tot prep 1.3 = A * OR_cli + B * OR_otc
# it looks like A = 1, B = 2, tot = 1.5

# first: get only OTC best == 1.3
# test with 0 - 0.75


cli_ors <- c(0,    0.3, 0.5, 0.7, 0.9, 1.1, 1.3)
otc_ors <- c(0.75, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
tmp_sc_names <- paste0("otc_cont_cli", cli_ors, "_otc", otc_ors)
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["otc_cont"]] <- d_base_otc_best |>
  slice_sample(n = length(cli_ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], cli_ors),
    prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], cli_ors),
    prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], cli_ors),
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], otc_ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], otc_ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], otc_ors)
  )

sc_ls <- lapply(sc_df_ls, EpiModel::create_scenario_list)
scenarios_list <- Reduce(c, sc_ls, init = list())
