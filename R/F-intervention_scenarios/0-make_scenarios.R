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

sc_ls[["baseline"]] <- tibble(
  .scenario.id    = paste0("baseline"),
  .at             = intervention_start,
)

sc_ls[["otc_same"]] <- tibble(
  .scenario.id    = paste0("otc_same_", 1),
  .at             = intervention_start,
  prep.otc.hard.indications = 1,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  prep.otc.start.rate_1 = param$prep.start.rate[1],
  prep.otc.start.rate_2 = param$prep.start.rate[2],
  prep.otc.start.rate_3 = param$prep.start.rate[3]
)

sc_ls[["otc_relaxed"]] <- tibble(
  .scenario.id    = paste0("otc_relaxed_", 1),
  .at             = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.always.sti.tst = 1,
  prep.otc.always.hiv.tst = 1,
  prep.otc.start.rate_1 = param$prep.start.rate[1],
  prep.otc.start.rate_2 = param$prep.start.rate[2],
  prep.otc.start.rate_3 = param$prep.start.rate[3]
)

sc_ls[["otc_free"]] <- tibble(
  .scenario.id    = paste0("otc_free_", 1),
  .at             = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.always.sti.tst = 0,
  prep.otc.always.hiv.tst = 0,
  prep.otc.start.rate_1 = param$prep.start.rate[1],
  prep.otc.start.rate_2 = param$prep.start.rate[2],
  prep.otc.start.rate_3 = param$prep.start.rate[3]
)

sc_ls[["otc_switch2otc"]] <- tibble(
  .scenario.id    = paste0("otc_switch2otc_", 1),
  .at             = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.always.sti.tst = 0,
  prep.otc.always.hiv.tst = 0,
  prep.otc.start.rate_1 = param$prep.start.rate[1],
  prep.otc.start.rate_2 = param$prep.start.rate[2],
  prep.otc.start.rate_3 = param$prep.start.rate[3],
  prep.std.switch.otc.prob = 0.05,
  prep.otc.switch.std.prob = 0
)

sc_ls[["otc_switch2std"]] <- tibble(
  .scenario.id    = paste0("otc_switch2std_", 1),
  .at             = intervention_start,
  prep.otc.hard.indications = 0,
  prep.otc.always.sti.tst = 0,
  prep.otc.always.hiv.tst = 0,
  prep.otc.start.rate_1 = param$prep.start.rate[1],
  prep.otc.start.rate_2 = param$prep.start.rate[2],
  prep.otc.start.rate_3 = param$prep.start.rate[3],
  prep.std.switch.otc.prob = 0,
  prep.otc.switch.std.prob = 0.05
)

sc_df <- bind_rows(sc_list)
readr::write_csv(sc_df, "data/input/scenarios.csv")
