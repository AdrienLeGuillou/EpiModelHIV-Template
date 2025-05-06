## 2. Netsim Module Development Script
##
## Example interactive epidemic simulation run script with basic
## parameterization and all parameters defined in data/input/model_parameters.xlsx`, with example of
## writing/debugging modules

# Restart R before running this script (Ctrl_Shift_F10 / Cmd_Shift_0)

# Setup ------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

source("R/shared_variables.R", local = TRUE)
source("R/B-netsim_explore/z-context.R", local = TRUE)

# load the local development version of the project
pkgload::load_all(EMHIVp_dir)
# library(EpiModelHIV)

# default theme for the plots
theme_set(theme_light())

# Process ----------------------------------------------------------------------

# set prep start to a low value to test the full model in a few steps
source("R/netsim_settings.R", local = TRUE)
est <- readRDS(path_to_est)


source("./R/utils-epi_debug.R", local = TRUE)

# Control settings
control <- control_msm(
  nsteps = year_steps / 4,
  .tracker.list = tk_list,
)

param$hbv.start <- 2

# Epidemic simulation
sim <- netsim(est, param, init, control)

# Simulation exploration (tidyverse)
d_sim <- as_tibble(sim)
glimpse(d_sim)


d_sim |>
  select(time, starts_with("gfr_otc")) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line()

# # Run in debug mode, more details and examples here:
# # https://github.com/EpiModel/EpiModeling/wiki/Writing-and-Debugging-EpiModel-Code
# debugonce(hivtrans_msm)
# sim <- netsim(est, param, init, control)
#
# # for advanced debugging: https://github.com/EpiModel/EpiModeling/wiki/Diagnostic-of-an-EpiModel-Module
