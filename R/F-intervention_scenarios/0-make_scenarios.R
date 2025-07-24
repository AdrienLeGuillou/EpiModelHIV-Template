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
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.gfr.low.risk.int = year_steps * 1,
  prep.otc.gfr.high.risk.int = year_steps / 2,
  prep.otc.gfr.risk.rng = 0,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob
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
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = 1 / (year_steps / 2), # mean time to test 6 months
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.gfr.low.risk.int = year_steps * 2, # mean time to GFR test: 2 years
  prep.otc.gfr.high.risk.int = year_steps * 2, # mean time to GFR test: 2 years
  prep.otc.gfr.risk.rng = 1, # rate based testing
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob
)
# ------------------------------------------------------------------------------

# Scenarios DF list ------------------------------------------------------------
sc_df_ls <- list()
sc_names <- c()

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

# Increase STD PrEP, still no OTC
tmp_sc_names <- paste0("no_otc_prep_or", c("125", "150", "175", "200"))
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(1.25, 1.50, 1.75, 2.0)
sc_df_ls[["no_otc_prep_or"]] <- sc_df_ls[["baseline"]] |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

# Replace STD with OTC PrEP that behaves like STD PrEP
tmp_sc_names <- paste0("only_otc_same_", c("100", "125", "150", "175", "200"))
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(1.0, 1.25, 1.50, 1.75, 2.0)
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

# Add OTC PrEP that behaves like STD PrEP
tmp_sc_names <- paste0("otc_same_", c("025", "050", "075", "100"))
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(0.25, 0.5, 0.75, 1.0)
sc_df_ls[["otc_same"]] <- d_base_same |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )

# Replace STD with OTC PrEP wiht best guess config
tmp_sc_names <- paste0(
  "only_otc_best_",
  c("025", "050", "075", "100", "125", "150")
)
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5)
sc_df_ls[["only_otc_"]] <- d_base_best |>
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

# Add OTC PrEP with "best guess" config
tmp_sc_names <- paste0(
  "otc_best_",
  c("012", "025", "037", "050", "062", "075", "087", "100")
)
sc_names <- c(sc_names, tmp_sc_names)
ors <- c(0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1.0)
sc_df_ls[["otc_best"]] <- d_base_best |>
  slice_sample(n = length(ors), replace = TRUE) |>
  mutate(
    .scenario.id = tmp_sc_names,
    prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
    prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
    prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
  )


# ints_ratios <- c(0.75, 0.5, 0.25, 1.25, 1.5, 1.75)
# sc_df_ls[["only_otc_relaxed_disc"]] <- tibble(
#   .scenario.id = paste0(
#     "only_otc_relaxed_disc_",
#     c("075", "050", "025", "125", "150", "175")
#   ),
#   .at = intervention_start,
#   prep.otc.hard.indications = 0,
#   prep.otc.gfr.stop = 1,
#   prep.start.rate_1 = 0,
#   prep.start.rate_2 = 0,
#   prep.start.rate_3 = 0,
#   prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
#   prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
#   prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
#   prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
#   prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
#   prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
#   prep.otc.discont.int_1 = param$prep.discont.int[1] * ints_ratios,
#   prep.otc.discont.int_2 = param$prep.discont.int[2] * ints_ratios,
#   prep.otc.discont.int_3 = param$prep.discont.int[3] * ints_ratios,
#   prep.otc.tst.int = param$prep.tst.int,
#   prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
#   prep.std.switch.otc.prob = 0,
#   prep.otc.switch.std.prob = 0,
#   prep.otc.always.sti.tst = 1,
#   prep.otc.always.hiv.tst = 1,
#   sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
#   sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
#   sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
#   prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
# )
#
# # TODO: high adherence scenarios:
# #   1. define new adhr dist
# #   2. implement adhr reassign (upon restart, not interv)
#
# ints <- c(1, 2, 5, Inf)
# sc_df_ls[["only_otc_relaxed_gfr_int"]] <- tibble(
#   .scenario.id = paste0("only_otc_relaxed_gfr_int", c("1", "2", "5", "Inf")),
#   .at = intervention_start,
#   prep.otc.hard.indications = 0,
#   prep.otc.gfr.stop = 1,
#   prep.otc.gfr.low.risk.int = ints * 52,
#   prep.otc.gfr.high.risk.int = ints * 52,
#   prep.start.rate_1 = 0,
#   prep.start.rate_2 = 0,
#   prep.start.rate_3 = 0,
#   prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
#   prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
#   prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
#   prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
#   prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
#   prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
#   prep.otc.discont.int_1 = param$prep.discont.int[1],
#   prep.otc.discont.int_2 = param$prep.discont.int[2],
#   prep.otc.discont.int_3 = param$prep.discont.int[3],
#   prep.otc.tst.int = param$prep.tst.int,
#   prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
#   prep.std.switch.otc.prob = 0,
#   prep.otc.switch.std.prob = 0,
#   prep.otc.always.sti.tst = 1,
#   prep.otc.always.hiv.tst = 1,
#   sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
#   sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
#   sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
#   prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
# )
#
# tst_ints <- c(13, 26, 52)
# sc_df_ls[["only_otc_relaxed_hiv_tst_ints"]] <- tibble(
#   .scenario.id = paste0("only_otc_relaxed_hiv_tst_ints_", tst_ints),
#   .at = intervention_start,
#   prep.otc.hard.indications = 0,
#   prep.otc.always.sti.tst = 0,
#   prep.otc.always.hiv.tst = 0,
#   prep.otc.tst.int = tst_ints,
#   sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
#   sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
#   prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
#   prep.otc.gfr.stop = 0,
#   prep.start.rate_1 = 0,
#   prep.start.rate_2 = 0,
#   prep.start.rate_3 = 0,
#   prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
#   prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
#   prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
#   prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
#   prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
#   prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
#   prep.otc.discont.int_1 = param$prep.discont.int[1],
#   prep.otc.discont.int_2 = param$prep.discont.int[2],
#   prep.otc.discont.int_3 = param$prep.discont.int[3],
#   prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
#   prep.std.switch.otc.prob = 0,
#   prep.otc.switch.std.prob = 0,
#   sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
# )
#
# ors <- c(2 / 3, 1 / 2, 1 / 3)
# sc_df_ls[["only_otc_relaxed_sti_screen"]] <- tibble(
#   .scenario.id = paste0("only_otc_relaxed__sti_screen_", c("06", "05", "03")),
#   .at = intervention_start,
#   prep.otc.hard.indications = 0,
#   prep.otc.always.sti.tst = 0,
#   prep.otc.always.hiv.tst = 0,
#   prep.otc.tst.int = param$prep.tst.int,
#   sti.screen.prep.otc.rate = apply_odds_ratio(param$sti.screen.prep.rate, ors),
#   sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
#   prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
#   prep.otc.gfr.stop = 0,
#   prep.start.rate_1 = 0,
#   prep.start.rate_2 = 0,
#   prep.start.rate_3 = 0,
#   prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
#   prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
#   prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
#   prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
#   prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
#   prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
#   prep.otc.discont.int_1 = param$prep.discont.int[1],
#   prep.otc.discont.int_2 = param$prep.discont.int[2],
#   prep.otc.discont.int_3 = param$prep.discont.int[3],
#   prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
#   prep.std.switch.otc.prob = 0,
#   prep.otc.switch.std.prob = 0,
#   sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
# )
#
# # PrEP ADHR changes
# shifts <- c(-20, -10, -5, 5, 10, 20) / 100
# adhrs <- vapply(
#   shifts,
#   EpiModelHIV::reallocate_pcp,
#   numeric(3),
#   in.pcp = param$prep.adhr.dist
# )
#
# sc_df_ls[["only_otc_relaxed_adhr"]] <- tibble(
#   .scenario.id = paste0(
#     "only_otc_relaxed__adhr_",
#     c("m20", "m10", "m05", "p05", "p10", "p20")
#   ),
#   .at = intervention_start,
#   prep.otc.hard.indications = 0,
#   prep.otc.always.sti.tst = 0,
#   prep.otc.always.hiv.tst = 0,
#   prep.otc.tst.int = param$prep.tst.int,
#   sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
#   sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
#   prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
#   prep.otc.gfr.stop = 0,
#   prep.start.rate_1 = 0,
#   prep.start.rate_2 = 0,
#   prep.start.rate_3 = 0,
#   prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
#   prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
#   prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
#   prep.otc.adhr.dist_1 = adhrs[1, ],
#   prep.otc.adhr.dist_2 = adhrs[2, ],
#   prep.otc.adhr.dist_3 = adhrs[3, ],
#   prep.otc.discont.int_1 = param$prep.discont.int[1],
#   prep.otc.discont.int_2 = param$prep.discont.int[2],
#   prep.otc.discont.int_3 = param$prep.discont.int[3],
#   prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
#   prep.std.switch.otc.prob = 0,
#   prep.otc.switch.std.prob = 0,
#   sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
# )
#
#
# # quantile - prob (p) that event occurs after interval (i)
# i2r_p <- function(i, p) 1 - (1 - p)^(1 / i)
# r2i_p <- function(r, p) log(1 - p, base = 1 - r)
#
# sw_year <- c(0.1, 0.05, 0.01)
# sw_p <- i2r_p(52, sw_year)
# sc_df_ls[["otc_switch2std"]] <- tibble(
#   .scenario.id = paste0("only_otc_relaxed_switch2std_", c("10", "05", "01")),
#   .at = intervention_start,
#   prep.otc.hard.indications = 0,
#   prep.otc.gfr.stop = 1,
#   prep.start.rate_1 = 0,
#   prep.start.rate_2 = 0,
#   prep.start.rate_3 = 0,
#   prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
#   prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
#   prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
#   prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
#   prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
#   prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
#   prep.otc.discont.int_1 = param$prep.discont.int[1],
#   prep.otc.discont.int_2 = param$prep.discont.int[2],
#   prep.otc.discont.int_3 = param$prep.discont.int[3],
#   prep.otc.tst.int = param$prep.tst.int,
#   prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
#   prep.std.switch.otc.prob = 0,
#   prep.otc.always.sti.tst = 1,
#   prep.otc.always.hiv.tst = 1,
#   sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
#   sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
#   sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
#   prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
#   prep.otc.switch.std.prob = sw_p
# )
#
# or_best_guess <- 0.6
# discont_best_guess <- 1.5
# sc_df_ls[["otc_best_guess"]] <- tibble(
#   .scenario.id = "otc_best_guess",
#   .at = intervention_start,
#   prep.otc.hard.indications = 0,
#   prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_best_guess),
#   prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_best_guess),
#   prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_best_guess),
#   prep.otc.start.rate_1 = apply_odds_ratio(
#     param$prep.start.rate[1],
#     or_best_guess
#   ),
#   prep.otc.start.rate_2 = apply_odds_ratio(
#     param$prep.start.rate[2],
#     or_best_guess
#   ),
#   prep.otc.start.rate_3 = apply_odds_ratio(
#     param$prep.start.rate[3],
#     or_best_guess
#   ),
#   prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
#   prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
#   prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
#   prep.otc.discont.int_1 = param$prep.discont.int[1] * discont_best_guess,
#   prep.otc.discont.int_2 = param$prep.discont.int[2] * discont_best_guess,
#   prep.otc.discont.int_3 = param$prep.discont.int[3] * discont_best_guess,
#   prep.otc.tst.int = year_steps / 2,
#   prep.otc.risk.reassess.int = 1, #NOTE: is that ok?
#   prep.std.switch.otc.prob = 0,
#   prep.otc.switch.std.prob = 0,
#   prep.otc.always.sti.tst = 0,
#   prep.otc.always.hiv.tst = 0,
#   sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
#   sti.screen.prep.otc.rate = 1 / 26, # mean int 6 month
#   sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
#   prep.otc.gfr.low.risk.int = 2 * 52,
#   prep.otc.gfr.high.risk.int = 2 * 52,
#   prep.otc.gfr.risk.rng = 1,
#   prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob
# )
#
# disc_ints <- c(0.75, 0.5, 0.25, 1.25, 1.5, 1.75)
# sc_df_ls[["otc_best_guess_disc_"]] <- sc_df_ls[["otc_best_guess"]] |>
#   slice_sample(n = length(disc_ints), replace = TRUE) |>
#   mutate(
#     .scenario.id = paste0(
#       "best_disc_",
#       c("075", "050", "025", "125", "150", "175")
#     ),
#     prep.otc.discont.int_1 = prep.otc.discont.int_1 * disc_ints,
#     prep.otc.discont.int_2 = prep.otc.discont.int_2 * disc_ints,
#     prep.otc.discont.int_3 = prep.otc.discont.int_3 * disc_ints,
#   )
#
#
# # Same as best but with prep intervals for GFR. (In an RNG way)
# sc_df_ls[["otc_best_guess_gfr_same"]] <- sc_df_ls[["otc_best_guess"]] |>
#   mutate(
#     .scenario.id = "best_gfr_same_int",
#     prep.otc.gfr.low.risk.int = 52,
#     prep.otc.gfr.high.risk.int = 26,
#   )
#
#
# gfr_ints <- c(1, 3, 5)
# sc_df_ls[["otc_best_guess_gfrs"]] <- sc_df_ls[["otc_best_guess"]] |>
#   slice_sample(n = length(gfr_ints), replace = TRUE) |>
#   mutate(
#     .scenario.id = paste0("best_gfr_int_", gfr_ints),
#     prep.otc.gfr.low.risk.int = gfr_ints * 52,
#     prep.otc.gfr.high.risk.int = gfr_ints * 52,
#   )
#
# # Both sti & hiv tests
# test_ints <- c(1 / 4, 1, 2)
# sc_df_ls[["otc_best_guess_tests"]] <- sc_df_ls[["otc_best_guess"]] |>
#   slice_sample(n = length(test_ints), replace = TRUE) |>
#   mutate(
#     .scenario.id = paste0("best_tests_int_", c("04", "12", "24")),
#     sti.screen.prep.otc.rate = 1 / (52 * test_ints),
#     prep.otc.tst.int = year_steps * test_ints,
#   )
#
# sc_df_ls[["otc_best_guess_adhr"]] <- sc_df_ls[["otc_best_guess"]] |>
#   slice_sample(n = length(shifts), replace = TRUE) |>
#   mutate(
#     .scenario.id = paste0(
#       "best_adhr_",
#       c("m20", "m10", "m05", "p05", "p10", "p20")
#     ),
#     prep.otc.adhr.dist_1 = adhrs[1, ],
#     prep.otc.adhr.dist_2 = adhrs[2, ],
#     prep.otc.adhr.dist_3 = adhrs[3, ],
#   )
#


# sc_df_ls <- sc_df_ls[c(
#   "only_otc_relaxed_adhr",
#   "otc_best_guess_adhr"
# )]

sc_ls <- lapply(sc_df_ls, EpiModel::create_scenario_list)
scenarios_list <- Reduce(c, sc_ls, init = list())
