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

oopts <- options(future.globals.maxSize = Inf)
if (context == "hpc" && exists("n_cores") && hpc_context) {
  future::plan("multicore", workers = n_cores)
}

# Process ----------------------------------------------------------------------

scenarios_tibble_dir <- fs::path(scenarios_dir, "merged_tibbles")
scenarios_info <- EpiModelHPC::get_scenarios_tibble_infos(scenarios_tibble_dir)
d_ref <- make_d_ref(fs::path(scenarios_tibble_dir, "df__baseline.rds"))

d_ls <- future.apply::future_lapply(
  # lapply(
  seq_len(nrow(scenarios_info)),
  \(i) process_one_scenario(scenarios_info[i, ], d_ref)
)

d_sc_raw <- dplyr::bind_rows(d_ls)
saveRDS(d_sc_raw, fs::path(output_dir, paste0("d_raw.rds")))

source("R/F-intervention_scenarios/labels.R", local = TRUE)

format_table(d_sc_raw, var_labels, format_patterns) |>
  write.csv(fs::path(output_dir, "table.csv"), row.names = FALSE)

# Make sub tables per scenario family ------------------------------------------

scs <- c(
  "only_otc_best",
  "only_otc_relaxed",
  "otc_best",
  "otc_mix",
  "base_only_otc_same"
)
for (i in seq_along(scs)) {
  sc <- scs[i]
  sc_ref <- paste0("df__", sc, "_some_hivtst_50")
  if (!fs::file_exists(sc_ref)) next

  d_ref <- make_d_ref(fs::path(scenarios_tibble_dir, sc_ref))

  sc_info <- scenarios_info |>
    filter(stringr::str_detect(scenario_name, paste0("^", sc, ".*")))

  d_ls <- future.apply::future_lapply(
    seq_len(nrow(sc_info)),
    \(i) process_one_scenario(sc_info[i, ], d_ref)
  )

  d_sc_raw <- dplyr::bind_rows(d_ls)
  saveRDS(d_sc_raw, fs::path(output_dir, paste0("d_raw_", sc, ".rds")))

  source("R/F-intervention_scenarios/labels.R", local = TRUE)

  format_table(d_sc_raw, var_labels, format_patterns) |>
    write.csv(
      fs::path(output_dir, paste0("table__", sc, ".csv")),
      row.names = FALSE
    )
}