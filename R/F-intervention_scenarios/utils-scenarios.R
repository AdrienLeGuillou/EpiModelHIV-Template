# Define the utilities and base DF for making the scenarios

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
otc_indic_or <- 0.31 # best guess with same indics
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
  prep.otc.always.sti.tst = 0, # no automatic STI test on start OTC
  prep.otc.always.hiv.tst = 0.5, # 50% HIV test on start OTC
  prep.otc.adhr.dist.realloc = FALSE,
  sti.prep.otc.tx.prob = param$sti.prep.tx.prob,
  sti.screen.prep.otc.rate = 1 / (year_steps / 2), # mean time to test 6 months
  sti.screen.rect.prep.otc.prob = param$sti.screen.rect.prep.prob,
  prep.otc.gfr.low.risk.int = year_steps * 2, # mean time to GFR test: 2 years
  prep.otc.gfr.high.risk.int = year_steps * 2, # mean time to GFR test: 2 years
  prep.otc.gfr.risk.rng = 1, # rate based testing
  prep.otc.hbv.flare.prob = param$prep.hbv.flare.prob
)

d_base_indic <- d_base_best |>
  mutate(
    prep.otc.risk.reassess.int = param$prep.risk.reassess.int,
    prep.otc.hard.indications = 1,
  )

d_base_sdur <- d_base_best |>
  mutate(
    prep.otc.discont.int_1 = param$prep.discont.int[1],
    prep.otc.discont.int_2 = param$prep.discont.int[2],
    prep.otc.discont.int_3 = param$prep.discont.int[3]
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

tmp_or <- otc_indic_or
d_base_otc_indic <- d_base_indic |>
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