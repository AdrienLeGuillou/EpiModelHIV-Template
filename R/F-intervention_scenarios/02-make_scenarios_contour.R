# Setup ------------------------------------------------------------------------
library(EpiModelHIV)
library(dplyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/utils-scenarios.R", local = TRUE)

# ors for various coverages
# ors <- tibble(
#   cli = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 1, 1.1, 1.3, 1.52),
#   otc = c(0.85, 0.777, 0.725, 0.65, 0.6, 0.53, 0.475, 0.42, 0.3, 0.25, 0.2, 0.1, 0)
# )

# ORs Best
# ors_otc <- c(
#   0,
#   0.03027552,
#   0.06595926,
#   0.10430880,
#   0.14680892,
#   0.19494439,
#   0.25019997,
#   0.31406043,
#   0.38801056,
#   0.47353511,
#   0.57211886
# )

# ORs Indic
ors_otc <- c(
  0,
  0.03780670,
  0.08089587,
  0.12802123,
  0.18089827,
  0.24124246,
  0.31076927,
  0.39119419,
  0.48423269,
  0.59160025,
  0.71501234
)

ors <- tibble(
  cli = 1,
  otc = ors_otc
)
ors$indexes <- seq_len(nrow(ors))

hiv_tests <- seq(0, 1, 0.1)

scs <- expand.grid(indexes = ors$indexes, prep.otc.always.hiv.tst = hiv_tests)
scs <- left_join(scs, ors, by = "indexes") |>
  select(-indexes) |>
  mutate(
    .at = 1,
    .scenario.id = paste0(
      "plot_indics_cli",
      cli,
      "_otc",
      otc,
      "_tst",
      prep.otc.always.hiv.tst
    ),
    prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], cli),
    prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], cli),
    prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], cli),
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], otc),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], otc),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], otc)
  ) |>
  select(-c(otc, cli))

scs <- slice_sample(d_base_best, n = length(nrow(scs)), replace = TRUE) |>
  select(
    -c(
      .at,
      prep.otc.always.hiv.tst,
      starts_with("prep.start.rate"),
      starts_with("prep.otc.start.rate")
    )
  ) |>
bind_cols(scs)

EpiModel::create_scenario_list
scenarios_list <- EpiModel::create_scenario_list(scs)