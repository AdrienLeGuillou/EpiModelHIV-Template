## 1. Intervention Scenarios Playground
##
## Example interactive epidemic simulation run script with more complex
## parameterization and parameters defined in spreadsheet, with example of
## running model scenarios defined with data-frame approach

# Restart R before running this script (Ctrl_Shift_F10 / Cmd_Shift_0)

# Setup ------------------------------------------------------------------------
library(EpiModelHIV)
library(dplyr)

source("R/shared_variables.R", local = TRUE)
# hpc_context = TRUE
source("R/F-intervention_scenarios/z-context.R", local = TRUE)

# Process ----------------------------------------------------------------------

# Necessary files
source("R/netsim_settings.R", local = TRUE)

# Control settings
control <- control_msm(
  start          = restart_time,
  nsteps         = restart_time + 10, #intervention_end,
  initialize.FUN = reinit_msm,
  debug          = TRUE,
  .tracker.list  = EpiModelHIV::make_calibration_trackers(),
  verbose        = TRUE
)

# Define test scenarios
# scenarios_df <- readr::read_csv(fs::path(input_dir, "scenarios.csv"))
#
# glimpse(scenarios_df)
# scenarios_list <- EpiModel::create_scenario_list(scenarios_df)
source("./R/F-intervention_scenarios/0-make_scenarios.R", local = TRUE)

# param$hbv.init.perc <- c(0.7, 0.6, 0.5)

EpiModelHPC::netsim_scenarios(
  path_to_restart, param, init, control,
  scenarios_list = scenarios_list[1],
  # scenarios_list = NULL,
  n_rep = 2,
  n_cores = 2,
  output_dir = scenarios_dir
)
fs::dir_ls(scenarios_dir)

est <- readRDS(path_to_restart)
est$epi <- list()
saveRDS(est, path_to_restart)

control$nsims = 2
control$ncores = 2
netsim(est, param, init, control)

# merge the simulations. Keeping one `tibble` per scenario
EpiModelHPC::merge_netsim_scenarios_tibble(
  sim_dir = scenarios_dir,
  output_dir = fs::path(scenarios_dir, "merged_tibbles"),
  steps_to_keep = intervention_end - intervention_start
)

# Convert to data frame
d_path <- fs::dir_ls(fs::path(scenarios_dir, "merged_tibbles"))[[1]]
d_sim <- readRDS(d_path)

glimpse(d_sim)
head(d_sim)

## Clean folder
# fs::dir_delete(sc_test_dir)
