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

# Process ----------------------------------------------------------------------

source("R/netsim_settings.R", local = TRUE)

prob_to_log_odds <- function(p) log(p / (1 - p))
log_odds_to_prob <- function(x) 1 / (1 + exp(-x))
apply_odds_ratio <- function(p, or) {
  log_odds_to_prob(prob_to_log_odds(p) + log(or))
}

# # Utility functions
# apply_or <- function(p, or) plogis(qlogis(p) + log(or))
# ors <- c(lo = 1 / 4, base = 1, hi = 4)
# interv_param <- c("test" = "hiv.test.rate", "treat" = "tx.init.rate")
#
# sc_list <- list()
#
# for (or_test in ors) {
#   for (or_tx in ors) {
#     sc_name <- paste0("test_", or_test, "_treat_", or_tx)
#     sc_list[[sc_name]] <- tibble(
#       .scenario.id    = sc_name,
#       .at             = intervention_start,
#       hiv.test.rate_1 = apply_or(param$hiv.test.rate[[1]], or_test),
#       hiv.test.rate_2 = apply_or(param$hiv.test.rate[[2]], or_test),
#       hiv.test.rate_3 = apply_or(param$hiv.test.rate[[2]], or_test),
#       tx.init.rate_1 =  apply_or(param$tx.init.rate[[1]], or_tx),
#       tx.init.rate_2 =  apply_or(param$tx.init.rate[[2]], or_tx),
#       tx.init.rate_3 =  apply_or(param$tx.init.rate[[3]], or_tx)
#     )
#   }
# }

or_same <- 2 / 3 # NOTE: assumed OR to get to the same N_on_PrEP

sc_df_ls <- list()

sc_df_ls[["no_otc"]] <- tibble(
  .scenario.id = paste0("baseline"),
  .at = intervention_start,
  prep.otc.start.rate_1 = 0,
  prep.otc.start.rate_2 = 0,
  prep.otc.start.rate_3 = 0,
)

ors <- c(1.5, 2.0)
sc_df_ls[["no_otc_prep_or"]] <- tibble(
  .scenario.id = paste0("no_otc_prep_or", c("15", "20")),
  .at = intervention_start,
  prep.otc.start.rate_1 = 0,
  prep.otc.start.rate_2 = 0,
  prep.otc.start.rate_3 = 0,
  prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
  prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
  prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors)
)

ors <- c(0.5, 2 / 3, 1, 1.5, 2.0)
sc_df_ls[["otc_same"]] <- tibble(
  .scenario.id = paste0("otc_same_", c("05", "06", "10", "15", "20")),
  .at = intervention_start,
  prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
  prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
  prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors),
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1],
  prep.otc.discont.int_2 = param$prep.discont.int[2],
  prep.otc.discont.int_3 = param$prep.discont.int[3],
  prep.otc.tst.int = param$prep.tst.int,
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  prep.otc.hard.indications = 1,
  prep.otc.gfr.stop = 1,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
)

ors <- c(0.5, 2 / 3, 1, 1.5, 2.0)
sc_df_ls[["otc_relaxed"]] <- tibble(
  .scenario.id = paste0("otc_relaxed_", c("05", "06", "10", "15", "20")),
  .at = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.gfr.stop = 1,
  prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
  prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
  prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors),
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1],
  prep.otc.discont.int_2 = param$prep.discont.int[2],
  prep.otc.discont.int_3 = param$prep.discont.int[3],
  prep.otc.tst.int = param$prep.tst.int,
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
)

sc_df_ls[["only_otc_same"]] <- tibble(
  .scenario.id = paste0("only_otc_same_", 1),
  .at = intervention_start,
  prep.start.rate_1 = 0,
  prep.start.rate_2 = 0,
  prep.start.rate_3 = 0,
  prep.otc.start.rate_1 = param$prep.start.rate[1],
  prep.otc.start.rate_2 = param$prep.start.rate[2],
  prep.otc.start.rate_3 = param$prep.start.rate[3],
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1],
  prep.otc.discont.int_2 = param$prep.discont.int[2],
  prep.otc.discont.int_3 = param$prep.discont.int[3],
  prep.otc.tst.int = param$prep.tst.int,
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  prep.otc.hard.indications = 1,
  prep.otc.gfr.stop = 1,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
)

ors <- c(0.5, 2 / 3, 1, 1.5, 2.0)
sc_df_ls[["only_otc_relaxed_base"]] <- tibble(
  .scenario.id = paste0(
    "only_otc_relaxed_base_",
    c("05", "06", "10", "15", "20")
  ),
  .at = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.gfr.stop = 1,
  prep.start.rate_1 = 0,
  prep.start.rate_2 = 0,
  prep.start.rate_3 = 0,
  # TODO: pick the value that leads to the same *number* of PrEP users: or_same
  prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], ors),
  prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], ors),
  prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], ors),
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1],
  prep.otc.discont.int_2 = param$prep.discont.int[2],
  prep.otc.discont.int_3 = param$prep.discont.int[3],
  prep.otc.tst.int = param$prep.tst.int,
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
)

ints_ratios <- c(0.75, 0.5, 0.25, 1.25, 1.5, 1.75)
sc_df_ls[["only_otc_relaxed_disc"]] <- tibble(
  .scenario.id = paste0(
    "only_otc_relaxed_disc_",
    c("075", "050", "025", "125", "150", "175")
  ),
  .at = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.gfr.stop = 1,
  prep.start.rate_1 = 0,
  prep.start.rate_2 = 0,
  prep.start.rate_3 = 0,
  prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
  prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
  prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1] * ints_ratios,
  prep.otc.discont.int_2 = param$prep.discont.int[2] * ints_ratios,
  prep.otc.discont.int_3 = param$prep.discont.int[3] * ints_ratios,
  prep.otc.tst.int = param$prep.tst.int,
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
)

# TODO: high adherence scenarios:
#   1. define new adhr dist
#   2. implement adhr reassign (upon restart, not interv)

ints <- c(1, 2, 5, Inf)
sc_df_ls[["only_otc_relaxed_gfr_int"]] <- tibble(
  .scenario.id = paste0("only_otc_relaxed_gfr_int", c("1", "2", "5", "Inf")),
  .at = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.gfr.stop = 1,
  prep.otc.gfr.low.risk.int = ints * 52,
  prep.otc.gfr.high.risk.int = ints * 52,
  prep.start.rate_1 = 0,
  prep.start.rate_2 = 0,
  prep.start.rate_3 = 0,
  prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
  prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
  prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1],
  prep.otc.discont.int_2 = param$prep.discont.int[2],
  prep.otc.discont.int_3 = param$prep.discont.int[3],
  prep.otc.tst.int = param$prep.tst.int,
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
)

tst_ints <- c(13, 26, 52)
sc_df_ls[["only_otc_relaxed_hiv_tst_ints"]] <- tibble(
  .scenario.id = paste0("only_otc_relaxed_hiv_tst_ints_", tst_ints),
  .at = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.always.sti.tst = 0,
  prep.otc.always.hiv.tst = 0,
  prep.otc.tst.int = tst_ints,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
  prep.otc.gfr.stop = 0,
  prep.start.rate_1 = 0,
  prep.start.rate_2 = 0,
  prep.start.rate_3 = 0,
  prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
  prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
  prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1],
  prep.otc.discont.int_2 = param$prep.discont.int[2],
  prep.otc.discont.int_3 = param$prep.discont.int[3],
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
)

ors <- c(2 / 3, 1 / 2, 1 / 3)
sc_df_ls[["only_otc_relaxed_sti_screen"]] <- tibble(
  .scenario.id = paste0("only_otc_relaxed__sti_screen_", c("06", "05", "03")),
  .at = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.always.sti.tst = 0,
  prep.otc.always.hiv.tst = 0,
  prep.otc.tst.int = param$prep.tst.int,
  sti.screen.prep.otc.rate = apply_odds_ratio(param$sti.screen.prep.rate, ors),
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
  prep.otc.gfr.stop = 0,
  prep.start.rate_1 = 0,
  prep.start.rate_2 = 0,
  prep.start.rate_3 = 0,
  prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
  prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
  prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1],
  prep.otc.discont.int_2 = param$prep.discont.int[2],
  prep.otc.discont.int_3 = param$prep.discont.int[3],
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
)


sc_df_ls[["otc_free"]] <- tibble(
  .scenario.id = paste0("otc_free_", 1),
  .at = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.always.sti.tst = 0,
  prep.otc.always.hiv.tst = 0,
  prep.otc.start.rate_1 = param$prep.start.rate[1],
  prep.otc.start.rate_2 = param$prep.start.rate[2],
  prep.otc.start.rate_3 = param$prep.start.rate[3]
)

sc_df_ls[["only_otc_free"]] <- tibble(
  .scenario.id = paste0("only_otc_free_", 1),
  .at = intervention_start,
  prep.start.rate_1 = 0,
  prep.start.rate_2 = 0,
  prep.start.rate_3 = 0,
  prep.otc.hard.indications = 0,
  prep.otc.always.sti.tst = 0,
  prep.otc.always.hiv.tst = 0,
  prep.otc.start.rate_1 = param$prep.start.rate[1],
  prep.otc.start.rate_2 = param$prep.start.rate[2],
  prep.otc.start.rate_3 = param$prep.start.rate[3]
)

# sc_df_ls[["otc_switch2otc"]] <- tibble(
#   .scenario.id = paste0("otc_switch2otc_", 1),
#   .at = intervention_start,
#   prep.otc.hard.indications = 0,
#   prep.otc.always.sti.tst = 0,
#   prep.otc.always.hiv.tst = 0,
#   prep.otc.start.rate_1 = param$prep.start.rate[1],
#   prep.otc.start.rate_2 = param$prep.start.rate[2],
#   prep.otc.start.rate_3 = param$prep.start.rate[3],
#   prep.std.switch.otc.prob = 0.05,
#   prep.otc.switch.std.prob = 0
# )

# quantile - prob (p) that event occurs after interval (i)
i2r_p <- function(i, p) 1 - (1 - p)^(1 / i)
r2i_p <- function(r, p) log(1 - p, base = 1 - r)

sw_year <- c(0.1, 0.05, 0.01)
sw_p <- i2r_p(52, sw_year)
sc_df_ls[["otc_switch2std"]] <- tibble(
  .scenario.id = paste0("only_otc_relaxed_switch2std_", c("10", "05", "01")),
  .at = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.gfr.stop = 1,
  prep.start.rate_1 = 0,
  prep.start.rate_2 = 0,
  prep.start.rate_3 = 0,
  prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_same),
  prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_same),
  prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_same),
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1],
  prep.otc.discont.int_2 = param$prep.discont.int[2],
  prep.otc.discont.int_3 = param$prep.discont.int[3],
  prep.otc.tst.int = param$prep.tst.int,
  prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
  prep.std.switch.otc.prob = 0,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = param$sti.screen.prep.rate,
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
  prep.otc.switch.std.prob = sw_p
)

or_best_guess <- 0.6
discont_best_guess <- 1.5
sc_df_ls[["otc_best_guess"]] <- tibble(
  .scenario.id = "otc_best_guess",
  .at = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.gfr.stop = 1,
  prep.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_best_guess),
  prep.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_best_guess),
  prep.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_best_guess),
  prep.otc.start.rate_1 = apply_odds_ratio(param$prep.start.rate[1], or_best_guess),
  prep.otc.start.rate_2 = apply_odds_ratio(param$prep.start.rate[2], or_best_guess),
  prep.otc.start.rate_3 = apply_odds_ratio(param$prep.start.rate[3], or_best_guess),
  prep.otc.adhr.dist_1 = param$prep.adhr.dist[1],
  prep.otc.adhr.dist_2 = param$prep.adhr.dist[2],
  prep.otc.adhr.dist_3 = param$prep.adhr.dist[3],
  prep.otc.discont.int_1 = param$prep.discont.int[1] * discont_best_guess,
  prep.otc.discont.int_2 = param$prep.discont.int[2] * discont_best_guess,
  prep.otc.discont.int_3 = param$prep.discont.int[3] * discont_best_guess,
  prep.otc.tst.int = year_steps / 2,
  prep.otc.risk.reassess.int = 1, #NOTE: is that ok?
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0,
  prep.otc.always.sti.tst = 0,
  prep.otc.always.hiv.tst = 0,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = 1 / 26, # mean int 6 month
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob,
  prep.otc.gfr.low.risk.int = 2 * 52,
  prep.otc.gfr.high.risk.int = 2 * 52,
  prep.otc.gfr.risk.rng = 1,
)

# sc_df <- bind_rows(sc_ls)
# readr::write_csv(sc_df, "data/input/scenarios.csv")
sc_ls <- sc_ls[c("otc_best_guess", "only_otc_relaxed_disc")]

sc_ls <- lapply(sc_df_ls, EpiModel::create_scenario_list)
scenarios_list <- Reduce(c, sc_ls, init = list())
