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

# # Simply no OTC
# tmp_sc_names <- "baseline"
# sc_names <- c(sc_names, tmp_sc_names)
# sc_df_ls[["baseline"]] <- tibble(
#   .scenario.id = paste0("baseline"),
#   .at = intervention_start,
#   prep.otc.start.rate_1 = 0,
#   prep.otc.start.rate_2 = 0,
#   prep.otc.start.rate_3 = 0
# )
#
# # These make the number of anyprep be + 5%->50%
# ors <- c(
#   x1.05 = 0.0427757331296098,
#   x1.10 = 0.0805903516625563,
#   x1.15 = 0.120727256402551,
#   x1.20 = 0.162987492753426,
#   x1.25 = 0.207172106119017,
#   x1.30 = 0.253082141903155,
#   x1.35 = 0.300518645509675,
#   x1.40 = 0.349282662342408,
#   x1.45 = 0.399175237805191,
#   x1.50 = 0.449997417301854
# )[c("x1.10", "x1.20", "x1.30", "x1.40", "x1.50")]
# tmp_sc_names <- paste0("add_otc_best", names(ors))
# sc_names <- c(sc_names, tmp_sc_names)
# sc_df_ls[["add_otc_best"]] <- d_base_best |>
#   slice_sample(n = length(ors), replace = TRUE) |>
#   mutate(
#     .scenario.id = tmp_sc_names,
#     prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
#     prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
#     prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
#   )
#
# # These make the number of anyprep be + 5%->50%
# ors <- c(
#   x1.05 = 0.0520679281388087,
#   x1.10 = 0.0999954602099051,
#   x1.15 = 0.151512480593926,
#   x1.20 = 0.206166637399693,
#   x1.25 = 0.263505578736026,
#   x1.30 = 0.323076952711746,
#   x1.35 = 0.384428407435674,
#   x1.40 = 0.447107591016628,
#   x1.45 = 0.510662151563433,
#   x1.50 = 0.574639737184907
# )[c("x1.10", "x1.20", "x1.30", "x1.40", "x1.50")]
# tmp_sc_names <- paste0("add_otc_indics", names(ors))
# sc_names <- c(sc_names, tmp_sc_names)
# sc_df_ls[["add_otc_indics"]] <- d_base_indic |>
#   slice_sample(n = length(ors), replace = TRUE) |>
#   mutate(
#     .scenario.id = tmp_sc_names,
#     prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
#     prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
#     prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
#   )
#
# sc_ls <- lapply(sc_df_ls, EpiModel::create_scenario_list)
# scenarios_list <- Reduce(c, sc_ls, init = list())
#
# These make the number of anyprep be + 5%->50%
ors <- c(
  x1.05 = 0.0808281917472001,
  x1.10 = 0.158451685729375,
  x1.15 = 0.246881077657844,
  x1.20 = 0.342297258210962,
  x1.25 = 0.440881118067086,
  x1.30 = 0.538813547904572,
  x1.35 = 0.632275438401776,
  x1.40 = 0.717447680237051,
  x1.45 = 0.790511164088757,
  x1.50 = 0.847646780635249
# )[c("x1.10", "x1.20", "x1.30", "x1.40", "x1.50")]
)[c("x1.30")]
tmp_sc_names <- paste0("add_otc_same", names(ors))
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["add_otc_same"]] <- d_base_same |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

# These make the number of anyprep be + 5%->50%
ors <- c(
  x1.05 = 0.0560576092205693,
  x1.10 = 0.107715105354057,
  x1.15 = 0.162691130302938,
  x1.20 = 0.220466630000594,
  x1.25 = 0.280522550380405,
  x1.30 = 0.342339837375751,
  x1.35 = 0.405399436920012,
  x1.40 = 0.469182294946569,
  x1.45 = 0.533169357388803,
  x1.50 = 0.596841570180095
)[c("x1.10", "x1.20", "x1.30", "x1.40", "x1.50")]
tmp_sc_names <- paste0("add_otc_sdur", names(ors))
sc_names <- c(sc_names, tmp_sc_names)
sc_df_ls[["add_otc_sdur"]] <- d_base_sdur |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

sc_ls <- lapply(sc_df_ls, EpiModel::create_scenario_list)
scenarios_list <- Reduce(c, sc_ls, init = list())