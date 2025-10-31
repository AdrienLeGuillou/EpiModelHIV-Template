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

# Scenarios exploring changes to relaxed, best and mix --------------------
name_bases <- c(
  # "only_otc_relaxed_"
  # "base_only_otc_same_",
  # "only_otc_best_",
  #"otc_mix_",
  "otc_best_",
  "otc_indic_"
)
d_bases <- list(
  # d_base_only_otc_relaxed,
  # d_base_only_otc_same,
  # d_base_only_otc_best,
  # d_base_otc_mix,
  d_base_otc_best,
  d_base_otc_indic
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
    c("13", "26", "52", "104", "208") # default is 26
  )
  sc_names <- c(sc_names, tmp_sc_names)
  tst_ints <- 52 / c(4, 2, 1, 1 / 2, 1 / 4)
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
    c("13", "26", "52", "104", "208") # default is 26
  )
  sc_names <- c(sc_names, tmp_sc_names)
  tst_ints <- 52 / c(4, 2, 1, 1 / 2, 1 / 4)
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

# Scenarios exploring always hiv/sti test in "best" likes ----------------------
name_bases <- c(
  # "only_otc_best_",
  # "otc_mix_",
  "otc_best_",
  "otc_indic_"
)
d_bases <- list(
  # d_base_only_otc_best,
  # d_base_otc_mix,
  d_base_otc_best,
  d_base_otc_indic
)

for (i in seq_along(name_bases)) {
  hivtst_prob <- c(0, 0.25, 0.5, 0.75, 1)
  tmp_sc_names <- paste0(name_bases[i], "some_hivtst_", c(0, 25, 50, 75, 100))
  sc_names <- c(sc_names, tmp_sc_names)
  sc_df_ls[[paste0(name_bases[i], "some_hivtst")]] <- d_bases[[i]] |>
    slice_sample(n = length(hivtst_prob), replace = TRUE) |>
    mutate(
      .scenario.id = tmp_sc_names,
      prep.otc.always.hiv.tst = hivtst_prob
    )
}

sc_ls <- lapply(sc_df_ls, EpiModel::create_scenario_list)
scenarios_list <- Reduce(c, sc_ls, init = list())