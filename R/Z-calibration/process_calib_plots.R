## Process Calibration Plots
##
## Generate a the calibration plots objects (not the images) on the HPC. And
## make the data available to be downloaded for the image generation on the
## local machine. *Generating the actual images on the HPC is complex as it is a
## headless environment*
##
## This script should be called by the restart_point workflow

# Setup ------------------------------------------------------------------------
library(dplyr)

source("R/shared_variables.R", local = TRUE)
source("R/Z-calibration/z-context.R", local = TRUE)

d_calib <- readRDS(fs::path(calib_dir, "merged_tibbles", "df__empty_scenario.rds"))
targets <- EpiModelHIV::get_calibration_targets()

d_outs <- EpiModelHIV::mutate_calibration_targets(d_calib) |>
  mutate(sim = as.integer(as.factor(paste0(batch_number, "_", sim)))) |>
  select(sim, time, any_of(names(targets))) |>
  as.epi.data.frame()

saveRDS(d_outs, fs::path(calib_dir, "merged_tibbles", "df__calib_plot.rds"))
