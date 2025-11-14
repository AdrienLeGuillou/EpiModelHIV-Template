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

ors <- seq(0.025, 0.65, 0.025)

# Simply no OTC
tmp_sc_names <- "baseline"
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["baseline"]] <- tibble(
  .scenario.id = paste0("baseline"),
  .at = intervention_start,
  prep.otc.start.rate_1 = 0,
  prep.otc.start.rate_2 = 0,
  prep.otc.start.rate_3 = 0
)

tmp_sc_names <- paste0("add_no_otc", ors)
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["add_no_otc"]] <- sc_df_ls[["baseline"]] |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

tmp_sc_names <- paste0("add_otc_best", ors)
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["add_otc_best"]] <- d_base_best |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

tmp_sc_names <- paste0("add_otc_sdur", ors)
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["add_otc_sdur"]] <- d_base_sdur |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

tmp_sc_names <- paste0("add_otc_indics", ors)
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["add_otc_indics"]] <- d_base_indic |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

tmp_sc_names <- paste0("add_otc_same", ors)
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["add_otc_same"]] <- d_base_same |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

sc_ls <- lapply(sc_df_ls, EpiModel::create_scenario_list)
scenarios_list <- Reduce(c, sc_ls, init = list())