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

tmp_sc_names <- paste0("no_otc_prep_or", c("125", "150", "175", "200"))
tmp_sc_names <- paste0("no_otc_prep_or", c("152"))
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(1.25, 1.50, 1.75, 2.0)
ors <- c(1.52)
sc_df_ls[["no_otc_prep_or"]] <- sc_df_ls[["baseline"]] |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

# NOTE: to get the right OR for same coverage

# Replace STD with OTC PrEP that behaves like STD PrEP
tmp_sc_names <- paste0("only_otc_same_", c("100", "150", "1525", "155"))
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(1.0, 1.50, 1.525, 1.55)
sc_df_ls[["only_otc_same"]] <- d_base_same |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.start.rate_1 = 0,
    prep.start.rate_2 = 0,
    prep.start.rate_3 = 0,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

# NOTE: to get the right OR for same coverage

# Replace STD with OTC PrEP that behaves like STD PrEP with relaxed indications
ors <- c(0.6, 0.625, 0.65, 0.675, 0.7, 0.725, 0.75, 0.775, 0.8, 0.825, 0.85, 0.875, 0.9)
ors_n <- stringr::str_replace(ors, "\\.", "")
tmp_sc_names <- paste0("only_otc_relaxed_", ors_n)
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["only_otc_relaxed"]] <- d_base_only_otc_relaxed |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.start.rate_1 = 0,
    prep.start.rate_2 = 0,
    prep.start.rate_3 = 0,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

# NOTE: to get the right OR for same coverage

# Replace STD with OTC PrEP wiht best guess config
tmp_sc_names <- paste0(
  "only_otc_best_",
  c("057", "058", "059", "060", "061")
)
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(0.57, 0.58, 0.59, 0.60, 0.61)
sc_df_ls[["only_otc_best"]] <- d_base_best |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.start.rate_1 = 0,
    prep.start.rate_2 = 0,
    prep.start.rate_3 = 0,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

# NOTE: to get the right OR for +30% with added OTC

# Add OTC PrEP with "best guess" config
tmp_sc_names <- paste0(
  "otc_best_",
  c("012", "025", "037", "050")
)
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(0.125, 0.25, 0.375, 0.5)
sc_df_ls[["otc_best"]] <- d_base_best |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

# NOTE: to get the right OR for +30% with added OTC, same rate as STD

tmp_sc_names <- paste0(
  "otc_mix_",
  c("052", "054", "056", "058")
)
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(0.52, 0.54, 0.56, 0.58)
sc_df_ls[["otc_mix"]] <- d_base_best |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors),
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )