# 2. Netsim Module Development Script
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
  # .tracker.list = tk_list,
  nsteps = year_steps / 2,
  raw.output = TRUE,
  debug = TRUE
)

param$hbv.start <- 2
param$hbv.init.perc <- c(1, 1, 1)
param$prep.otc.hbv.flare.prob <- 0.5
param$prep.hbv.flare.prob <- 0.5
param$gfr.90.decline.rate <- c(0.01, 0.02)
param$gfr.60.decline.rate <- c(0.01, 0.02)

param$gfr.decline.prep.rate <- 100 * 3.8e-4 # 2 per 100 pyar
param$gfr.decline.age.gt50.or <- 6
param$gfr.decline.gfr.lt90.or <- 8.5
param$gfr.prep.recov.rate <- 0.1591036

param$tdf.resist.prep.prob <- 0.03
param$tdf.resist.hiv.prob <- 0.18
param$ftc.resist.prep.prob <- 0.03
param$ftc.resist.hiv.prob <- 0.18

param$sti.screen.rect.prep.otc.prob <- 1
param$sti.screen.prep.otc.rate <- 0.07692308

param$prep.otc.start.rate <- c(0.00554146, 0.004232283, 0.0066210)
param$prep.otc.adhr.dist <- c(0.089, 0.127, 0.784)
param$prep.otc.discont.int <- c(33.42, 57.48, 57.39)
param$prep.otc.tst.int <- 12.8571
param$prep.otc.sti.screen.int <- 26
param$prep.otc.sti.tx.prob <- 1
param$prep.otc.risk.reassess.int <- 0
param$prep.std.switch.otc.prob <- 0
param$prep.otc.switch.std.prob <- 0
param$prep.otc.hard.indications <- 0
param$prep.otc.always.sti.tst <- 0
param$prep.otc.always.hiv.tst <- 0

# Epidemic simulation
dat_list <- netsim(est, param, init, control)
cat("\n")
dat <- dat_list[[1]]
sim <- process_out.net(dat_list)

# Simulation exploration (tidyverse)
d_sim <- as_tibble(sim)
glimpse(tail(d_sim, 15))

d_sim$stop_std_prep


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
