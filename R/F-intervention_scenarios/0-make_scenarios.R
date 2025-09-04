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


source("R/netsim_settings.R", local = TRUE)

prob_to_log_odds <- function(p) log(p / (1 - p))
log_odds_to_prob <- function(x) 1 / (1 + exp(-x))
apply_odds_ratio <- function(p, or) {
  log_odds_to_prob(prob_to_log_odds(p) + log(or))
}

################################################################################
### corresp OR start rate to Percentage increase for STD PrEP:
###   ors <- c(1.25, 1.50, 1.75, 2.0)
###         ~ +15%, +30%, +40%, +50%
###   for `otc_best` we aim for +30% any PrEP -> 16 500 users `otc_best_025`
###   for `otc_mix` -> >0.5 && <0.6 (best guess 0.52)
################################################################################
only_otc_relaxed_or <- 0.8563
only_otc_best_or <- 0.595
otc_best_or <- 0.25
otc_mix_or <- 0.52

# Base DF for scenarios: -------------------------------------------------------
#
# # OTC == STD
d_base_same <- tibble(
  .at = intervention_start,
  prep.otc.start.rate_1 = 0,
  prep.otc.start.rate_2 = 0,
  prep.otc.start.rate_3 = 0,
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1],
  prep.otc.discont.int_2 = param$prep.discont.int[2],
  prep.otc.discont.int_3 = param$prep.discont.int[3],
  prep.otc.tst.int = param$prep.tst.int,
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.otc.hard.indications = 1,
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  prep.otc.adhr.dist.realloc = FALSE,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.gfr.low.risk.int = year_steps * 1,
  prep.otc.gfr.high.risk.int = year_steps / 2,
  prep.otc.gfr.risk.rng = 0,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob
)

d_base_relaxed <- d_base_same |>
  mutate(
    prep.otc.hard.indications = 0,
    prep.otc.risk.reassess.int = 0
  )

# OTC best guess scenario
d_base_best <- tibble(
  .at = intervention_start,
  prep.otc.start.rate_1 = 0,
  prep.otc.start.rate_2 = 0,
  prep.otc.start.rate_3 = 0,
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1] * 1.5, # higher discont int
  prep.otc.discont.int_2 = param$prep.discont.int[2] * 1.5, # higher discont int
  prep.otc.discont.int_3 = param$prep.discont.int[3] * 1.5, # higher discont int
  prep.otc.tst.int = year_steps / 2, # HIV test mean interval of 6 months
  prep.otc.risk.reassess.int = 0, # check indications on every step
  prep.otc.hard.indications = 0, # relaxed indications
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  prep.otc.always.sti.tst = 0, # no automatic test on start OTC
  prep.otc.always.hiv.tst = 0, # no automatic test on start OTC
  prep.otc.adhr.dist.realloc = FALSE,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = 1 / (year_steps / 2), # mean time to test 6 months
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.gfr.low.risk.int = year_steps * 2, # mean time to GFR test: 2 years
  prep.otc.gfr.high.risk.int = year_steps * 2, # mean time to GFR test: 2 years
  prep.otc.gfr.risk.rng = 1, # rate based testing
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob
)

d_base_only_otc_same <- d_base_same |>
  mutate(
    prep.start.rate_1 = 0,
    prep.start.rate_2 = 0,
    prep.start.rate_3 = 0,
    prep.otc.start.rate_1 = param$prep.start.rate[1],
    prep.otc.start.rate_2 = param$prep.start.rate[2],
    prep.otc.start.rate_3 = param$prep.start.rate[3]
  )

tmp_or <- only_otc_relaxed_or
d_base_only_otc_relaxed <- d_base_relaxed |>
  mutate(
    prep.start.rate_1 = 0,
    prep.start.rate_2 = 0,
    prep.start.rate_3 = 0,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], tmp_or),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], tmp_or),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], tmp_or)
  )

tmp_or <- only_otc_best_or
d_base_only_otc_best <- d_base_best |>
  mutate(
    prep.start.rate_1 = 0,
    prep.start.rate_2 = 0,
    prep.start.rate_3 = 0,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], tmp_or),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], tmp_or),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], tmp_or)
  )

tmp_or <- otc_best_or
d_base_otc_best <- d_base_best |>
  mutate(
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], tmp_or),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], tmp_or),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], tmp_or)
  )

tmp_or <- otc_mix_or
d_base_otc_mix <- d_base_best |>
  mutate(
    prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], tmp_or),
    prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], tmp_or),
    prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], tmp_or),
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], tmp_or),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], tmp_or),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], tmp_or)
  )
# ------------------------------------------------------------------------------

# Scenarios DF list ------------------------------------------------------------
sc_df_ls <- list()
sc_names <- c()

# # TODO: re-activate
#
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

# # NOTE: to get the right OR for +30%
#
# tmp_sc_names <- paste0("no_otc_prep_or", c("125", "150", "175", "200"))
# sc_names <- c(sc_names, tmp_sc_names)
# ors <- c(1.25, 1.50, 1.75, 2.0)
# sc_df_ls[["no_otc_prep_or"]] <- sc_df_ls[["baseline"]] |>
#   slice_sample(n = length(ors), replace = TRUE) |>
#   mutate(
#     .scenario.id = tmp_sc_names,
#     prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
#     prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
#     prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
#   )

# # NOTE: to get the right OR for same coverage
#
# # Replace STD with OTC PrEP that behaves like STD PrEP
# tmp_sc_names <- paste0("only_otc_same_", c("100", "150", "1525", "155"))
# sc_names <- c(sc_names, tmp_sc_names)
# ors <- c(1.0, 1.50, 1.525, 1.55)
# sc_df_ls[["only_otc_same"]] <- d_base_same |>
#   slice_sample(n = length(ors), replace = TRUE) |>
#   mutate(
#     .scenario.id = tmp_sc_names,
#     prep.start.rate_1 = 0,
#     prep.start.rate_2 = 0,
#     prep.start.rate_3 = 0,
#     prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
#     prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
#     prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
#   )

# # NOTE: to get the right OR for same coverage
#
# # Replace STD with OTC PrEP that behaves like STD PrEP with relaxed indications
# ors <- c(0.6, 0.625, 0.65, 0.675, 0.7, 0.725, 0.75, 0.775, 0.8, 0.825, 0.85, 0.875, 0.9)
# ors_n <- stringr::str_replace(ors, "\\.", "")
# tmp_sc_names <- paste0("only_otc_relaxed_", ors_n)
# sc_names <- c(sc_names, tmp_sc_names)
# sc_df_ls[["only_otc_relaxed"]] <- d_base_only_otc_relaxed |>
#   slice_sample(n = length(ors), replace = TRUE) |>
#   mutate(
#     .scenario.id = tmp_sc_names,
#     prep.start.rate_1 = 0,
#     prep.start.rate_2 = 0,
#     prep.start.rate_3 = 0,
#     prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
#     prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
#     prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
#   )

# # NOTE: to get the right OR for same coverage
#
# # Replace STD with OTC PrEP wiht best guess config
# tmp_sc_names <- paste0(
#   "only_otc_best_",
#   c("057", "058", "059", "060", "061")
# )
# sc_names <- c(sc_names, tmp_sc_names)
# ors <- c(0.57, 0.58, 0.59, 0.60, 0.61)
# sc_df_ls[["only_otc_best"]] <- d_base_best |>
#   slice_sample(n = length(ors), replace = TRUE) |>
#   mutate(
#     .scenario.id = tmp_sc_names,
#     prep.start.rate_1 = 0,
#     prep.start.rate_2 = 0,
#     prep.start.rate_3 = 0,
#     prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
#     prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
#     prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
#   )

# # NOTE: to get the right OR for +30% with added OTC
#
# # Add OTC PrEP with "best guess" config
# tmp_sc_names <- paste0(
#   "otc_best_",
#   c("012", "025", "037", "050")
# )
# sc_names <- c(sc_names, tmp_sc_names)
# ors <- c(0.125, 0.25, 0.375, 0.5)
# sc_df_ls[["otc_best"]] <- d_base_best |>
#   slice_sample(n = length(ors), replace = TRUE) |>
#   mutate(
#     .scenario.id = tmp_sc_names,
#     prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
#     prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
#     prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
#   )

# # NOTE: to get the right OR for +30% with added OTC, same rate as STD
#
# tmp_sc_names <- paste0(
#   "otc_mix_",
#   c("052", "054", "056", "058")
# )
# sc_names <- c(sc_names, tmp_sc_names)
# ors <- c(0.52, 0.54, 0.56, 0.58)
# sc_df_ls[["otc_mix"]] <- d_base_best |>
#   slice_sample(n = length(ors), replace = TRUE) |>
#   mutate(
#     .scenario.id = tmp_sc_names,
#     prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
#     prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
#     prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors),
#     prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
#     prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
#     prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
#   )

# Scenarios exploring changes to relaxed, best and mix --------------------
name_bases <- c(
  # "only_otc_relaxed_"
  # "only_otc_best_",
  # "otc_best_",
  # "otc_mix_",
  # "base_only_otc_same_",
  "only_otc_relaxed_"
)
d_bases <- list(
  d_base_only_otc_relaxed,
  d_base_only_otc_best,
  d_base_otc_best,
  d_base_otc_mix,
  d_base_only_otc_same
)

for (i in seq_along(name_bases)) {
  # Modify the discontinuation
  tmp_sc_names <- paste0(
    name_bases[i],
    "disc_",
    c("025", "050", "075", "100", "125", "150", "175")
  )
  sc_names <- c(sc_names, tmp_sc_names)
  ints_ratios <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75)
  sc_df_ls[[paste0(name_bases[i], "disc")]] <- d_bases[[i]] |>
    slice_sample(n = length(ints_ratios), replace = TRUE) |>
    mutate(
      .scenario.id = tmp_sc_names,
      prep.otc.discont.int_1 = prep.otc.discont.int_1 * ints_ratios,
      prep.otc.discont.int_2 = prep.otc.discont.int_2 * ints_ratios,
      prep.otc.discont.int_3 = prep.otc.discont.int_3 * ints_ratios
    )

  # Modify gfr testing
  tmp_sc_names <- paste0(
    name_bases[i],
    "gfr_",
    c("same", "1", "2", "3", "5", "Inf")
  )
  sc_names <- c(sc_names, tmp_sc_names)
  low_gfr_ints <- c(1, 1, 2, 3, 5, Inf)
  high_gfr_ints <- c(1 / 2, 1, 2, 3, 5, Inf)
  sc_df_ls[[paste0(name_bases[i], "gfr")]] <- d_bases[[i]] |>
    slice_sample(n = length(low_gfr_ints), replace = TRUE) |>
    mutate(
      .scenario.id = tmp_sc_names,
      prep.otc.gfr.low.risk.int = year_steps * low_gfr_ints,
      prep.otc.gfr.high.risk.int = year_steps * high_gfr_ints
    )

  # Modify hiv testing
  tmp_sc_names <- paste0(
    name_bases[i],
    "hivtst_",
    c("13", "26", "52") # default is 26
  )
  sc_names <- c(sc_names, tmp_sc_names)
  tst_ints <- c(13, 26, 52)
  sc_df_ls[[paste0(name_bases[i], "hivtst")]] <- d_bases[[i]] |>
    slice_sample(n = length(tst_ints), replace = TRUE) |>
    mutate(
      .scenario.id = tmp_sc_names,
      prep.otc.tst.int = tst_ints
    )

  # Modify STI testing
  tmp_sc_names <- paste0(
    name_bases[i],
    "stitst_",
    c("13", "26", "52") # default is 26
  )
  sc_names <- c(sc_names, tmp_sc_names)
  tst_ints <- c(13, 26, 52)
  sc_df_ls[[paste0(name_bases[i], "stitst")]] <- d_bases[[i]] |>
    slice_sample(n = length(tst_ints), replace = TRUE) |>
    mutate(
      .scenario.id = tmp_sc_names,
      sti.screen.prep.otc.rate = 1 / tst_ints
    )

  # Modify ADHR
  tmp_sc_names <- paste0(
    name_bases[i],
    "adhr_",
    c("m20", "m10", "m05", "base", "p05", "p10", "p20")
  )
  sc_names <- c(sc_names, tmp_sc_names)
  shifts <- c(-20, -10, -5, 0, 5, 10, 20) / 100
  adhrs <- vapply(
    shifts,
    EpiModelHIV::reallocate_pcp,
    numeric(3),
    in.pcp = param$prep.adhr.dist
  )
  sc_df_ls[[paste0(name_bases[i], "adhr")]] <- d_bases[[i]] |>
    slice_sample(n = length(shifts), replace = TRUE) |>
    mutate(
      .scenario.id = tmp_sc_names,
      prep.otc.adhr.dist.realloc = TRUE,
      prep.otc.adhr.dist_1 = adhrs[1, ],
      prep.otc.adhr.dist_2 = adhrs[2, ],
      prep.otc.adhr.dist_3 = adhrs[3, ],
    )
}

# # TODO: re-activate
#
# # Scenarios exploring always hiv/sti test in "best" likes ----------------------
# name_bases <- c(
#   "only_otc_best_",
#   "otc_best_",
#   "otc_mix_"
# )
# d_bases <- list(
#   d_base_only_otc_best,
#   d_base_otc_best,
#   d_base_otc_mix
# )
#
# for (i in seq_along(name_bases)) {
#   tmp_sc_names <- paste0( name_bases[i], "always_hivtst")
#   sc_names <- c(sc_names, tmp_sc_names)
#   sc_df_ls[[paste0(name_bases[i], "always_hivtst")]] <- d_bases[[i]] |>
#     slice_sample(n = 1, replace = TRUE) |>
#     mutate(
#       .scenario.id = tmp_sc_names,
#       prep.otc.always.hiv.tst = 1
#     )
#
#   tmp_sc_names <- paste0(name_bases[i], "always_stitst")
#   sc_names <- c(sc_names, tmp_sc_names)
#   sc_df_ls[[paste0(name_bases[i], "always_stitst")]] <- d_bases[[i]] |>
#     slice_sample(n = 1, replace = TRUE) |>
#     mutate(
#       .scenario.id = tmp_sc_names,
#       prep.otc.always.sti.tst = 1
#     )
#
#   tmp_sc_names <- paste0(name_bases[i], "always_bothtst")
#   sc_names <- c(sc_names, tmp_sc_names)
#   sc_df_ls[[paste0(name_bases[i], "always_bothtst")]] <- d_bases[[i]] |>
#     slice_sample(n = 1, replace = TRUE) |>
#     mutate(
#       .scenario.id = tmp_sc_names,
#       prep.otc.always.hiv.tst = 1,
#       prep.otc.always.sti.tst = 1
#     )
#
#   hivtst_prob <- c(0.25, 0.5, 0.75)
#   tmp_sc_names <- paste0(name_bases[i], "some_hivtst_", c(25, 50, 75))
#   sc_names <- c(sc_names, tmp_sc_names)
#   sc_df_ls[[paste0(name_bases[i], "some_hivtst")]] <- d_bases[[i]] |>
#     slice_sample(n = length(hivtst_prob), replace = TRUE) |>
#     mutate(
#       .scenario.id = tmp_sc_names,
#       prep.otc.always.hiv.tst = hivtst_prob
#     )
#
# }


# TODO: add switch scs?

# sc_df_ls <- sc_df_ls[c(
#   "baseline",
#   "only_otc_same"
# )]

sc_ls <- lapply(sc_df_ls, EpiModel::create_scenario_list)
scenarios_list <- Reduce(c, sc_ls, init = list())
