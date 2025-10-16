## HPC Workflow: Intervention scenarios
##
## Define a workflow to run the intervention scenarios, merge the output,
## produce formatted tables and plots

# Restart R before running this script (Ctrl_Shift_F10 / Cmd_Shift_0)

# Setup ------------------------------------------------------------------------
library(slurmworkflow)
library(EpiModelHPC)
library(EpiModelHIV)
library(dplyr)

hpc_context <- TRUE
source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/hpc_configs.R", local = TRUE)

max_cores <- 8

# Process ----------------------------------------------------------------------

source("R/netsim_settings.R", local = TRUE)

# Control settings
control <- control_msm(
  start = restart_time,
  nsteps = intervention_end,
  initialize.FUN = reinit_msm,
  debug = TRUE,
  .tracker.list = EpiModelHIV::make_calibration_trackers(),
  verbose = FALSE
)

# Workflow creation
wf <- make_em_workflow("indic_interventions", override = TRUE)

# Define test scenarios
source("./R/F-intervention_scenarios/03-make_scenarios_indic.R", local = TRUE)

wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_netsim_scenarios(
    path_to_restart,
    param,
    init,
    control,
    scenarios_list = scenarios_list,
    output_dir = scenarios_dir,
    n_rep = 64,
    n_cores = max_cores,
    max_array_size = 500,
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "mail-type" = "FAIL,TIME_LIMIT",
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "5G"
  )
)

# Process calibrations
wj <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_merge_netsim_scenarios_tibble(
    sim_dir = scenarios_dir,
    output_dir = fs::path(scenarios_dir, "merged_tibbles"),
    steps_to_keep = intervention_end - intervention_start,
    cols = dplyr::everything(),
    n_cores = max_cores,
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = 32,
    "time" = "06:00:00",
    "mem-per-cpu" = "5G"
  )
)

# make tables step
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "./R/F-intervention_scenarios/2-process_plot_data.R",
    args = list(
      hpc_context = TRUE,
      n_cores = max_cores
    ),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "mail-type" = "END",
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "5G"
  )
)
