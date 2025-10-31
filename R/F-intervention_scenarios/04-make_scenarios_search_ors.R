# Setup ------------------------------------------------------------------------
library(EpiModelHIV)
library(dplyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/utils-scenarios.R", local = TRUE)


# Scenarios DF list ------------------------------------------------------------
sc_df_ls <- list()
sc_names <- c()

# NOTE: to get the right OR for +30%

ors <- seq(0.25, 0.75, length.out = 10)
tmp_sc_names <- paste0("otc_sdur_", ors)
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["otc_sdur"]] <- d_base_sdur  |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

sc_ls <- lapply(sc_df_ls, EpiModel::create_scenario_list)
scenarios_list <- Reduce(c, sc_ls, init = list())