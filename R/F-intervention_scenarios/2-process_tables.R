## 2. Intervention Scenarios Process Tables
##
## Make the tables using the results of the simulations from the previous step
## locally or on the HPC (see `workflow-interventions.R`)

# Restart R before running this script (Ctrl_Shift_F10 / Cmd_Shift_0)

# Setup ------------------------------------------------------------------------
library(dplyr)
library(tidyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)

source("R/F-intervention_scenarios/outcomes.R", local = TRUE)

# Process ----------------------------------------------------------------------

scenarios_tibble_dir <- fs::path(scenarios_dir, "merged_tibbles")
# scenarios_tibble_dir <- "./data/run/sav_scs_07_30/merged_tibbles/"
scenarios_info <- EpiModelHPC::get_scenarios_tibble_infos(scenarios_tibble_dir)

d_ref <- make_d_ref(fs::path(scenarios_tibble_dir, "df__baseline.rds"))

d_ls <- future.apply::future_lapply(
  seq_len(nrow(scenarios_info)),
  \(i) process_one_scenario(scenarios_info[i, ], d_ref)
)

d_sc_raw <- dplyr::bind_rows(d_ls)
glimpse(d_sc_raw)

source("R/F-intervention_scenarios/labels.R", local = TRUE)

format_table(d_sc_raw, var_labels, format_patterns) |>
  write.csv(fs::path(output_dir, "table.csv"), row.names = FALSE)

# Make sub tables per scenario family ------------------------------------------

scs <- c(
  "only_otc_best",
  "only_otc_relaxed",
  "otc_best",
  "otc_best_mix",
  "only_otc_same"
)
for (i in seq_along(scs)) {
  sc <- scs[i]
  sc_ref <- paste0("df__", sc, "_adhr_base.rds")
  d_ref <- make_d_ref(fs::path(scenarios_tibble_dir, sc_ref))

  sc_info <- scenarios_info |>
    filter(stringr::str_detect(scenario_name, paste0("^", sc, ".*")))

  d_ls <- future.apply::future_lapply(
    seq_len(nrow(sc_info)),
    \(i) process_one_scenario(scenarios_info[i, ], d_ref)
  )

  d_sc_raw <- dplyr::bind_rows(d_ls)
  glimpse(d_sc_raw)

  source("R/F-intervention_scenarios/labels.R", local = TRUE)

  format_table(d_sc_raw, var_labels, format_patterns) |>
    write.csv(
      fs::path(output_dir, paste0("table__", sc, ".csv")),
      row.names = FALSE
    )
}
