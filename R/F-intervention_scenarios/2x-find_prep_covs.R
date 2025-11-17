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

mutate_outcomes <- function(d) {
  d |>
    mutate(
      # HIV
      ## Clinical PrEP
      lst_prep_num = prepCurr,
      lst_prep_otc_num = prep.otcCurr,
      lst_prep_any = prepCurr + prep.otcCurr,
      lst_prop_otc = prep.otcCurr / lst_prep_any,
      lst_prep_otc_std_indic_cov = prep.otc.std.indic / prep.otcCurr
    )
}

make_last_year_outcomes <- function(d) {
  d |>
    filter(time >= max(time) - year_steps) |>
    group_by(scenario_name, sim) |>
    summarise(across(starts_with("lst_"), \(x) mean(x, na.rm = TRUE))) |>
    ungroup()
}

process_one_scenario <- function(scenario_infos, d_ref) {
  d_sim <- readRDS(scenario_infos$file_path)
  d_sim <- mutate_outcomes(d_sim)
  d_sim <- mutate(d_sim, scenario_name = scenario_infos$scenario_name)
  make_last_year_outcomes(d_sim)
}

oopts <- options(future.globals.maxSize = Inf)
if (context == "hpc" && exists("n_cores") && hpc_context) {
  future::plan("multicore", workers = n_cores)
}

# Process ----------------------------------------------------------------------

scenarios_tibble_dir <- fs::path(scenarios_dir, "merged_tibbles")
scenarios_info <- EpiModelHPC::get_scenarios_tibble_infos(scenarios_tibble_dir)

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