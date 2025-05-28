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
  # .tracker.list = tk_list,
  nsteps = year_steps * 20,
  raw.output = TRUE,
  verbose = FALSE,
  debug = TRUE
)

# PrEP OTC
# param$prep.otc.start.rate <- c(0.00554146, 0.004232283, 0.0066210)
# param$prep.otc.adhr.dist <- c(0.089, 0.127, 0.784)
# param$prep.otc.discont.int <- c(33.42, 57.48, 57.39)
# param$prep.otc.tst.int <- 12.8571
# param$prep.otc.sti.screen.int <- 26
# param$prep.otc.sti.tx.prob <- 1
# param$prep.otc.risk.reassess.int <- 0
#   param$prep.otc.risk.reassess.int <- param$prep.risk.reassess.int
# param$prep.std.switch.otc.prob <- 0.02
# param$prep.otc.switch.std.prob <- 0.04
# param$prep.otc.hard.indications <- 1
# param$prep.otc.always.sti.tst <- 1
# param$prep.otc.always.hiv.tst <- 1
# param$sti.screen.prep.otc.rate <- 0.07692308
# param$sti.screen.rect.prep.otc.prob <- 1
# param$sti.prep.otc.tx.prob <- 1
# # GFR
# param$gfr.90.decline.rate <- c(0.001, 0.002)
# param$gfr.60.decline.rate <- c(0.001, 0.002)
# param$gfr.decline.prep.rate <- 10 * 3.8e-4 # 2 per 100 pyar
# param$gfr.decline.age.gt50.or <- 6
# param$gfr.decline.gfr.lt90.or <- 8.5
# param$gfr.prep.recov.rate <- 0.1591036
# # HBV
# param$hbv.start <- 2
# param$hbv.init.perc <- c(0.7, 0.6, 0.5)
# param$prep.otc.hbv.flare.prob <- 0.75
# param$prep.hbv.flare.prob <- 0.5
# # Resist
# param$tdf.resist.prep.prob <- 0.9
# param$tdf.resist.hiv.prob <- 0.9
# param$ftc.resist.prep.prob <- 0.7
# param$ftc.resist.hiv.prob <- 0.7

### Only PrEP STD or only PrEP OTC
param$prep.start.rate <- rep(0.01, 3)
param$prep.otc.start.rate <- rep(0.01, 3)
param$prep.otc.hard.indications <- 0
param$sti.screen.prep.otc.rate <- 0.07692308 ## NOTE: This one is 3 month (13 weeks)
param$prep.otc.sti.screen.int <- 26 ########### TODO: this one is never used!!
# No other tests than PrEP for HIV and STIs
param$hiv.test.rate <- rep(0, 3)
param$gono.screen.hivneg.rate <- 0
param$gono.screen.hivpos.rate <- 0
param$chla.screen.hivneg.rate <- 0
param$chla.screen.hivpos.rate <- 0
param$syph.screen.hivneg.rate <- 0
param$syph.screen.hivpos.rate <- 0
param$hbv.start <- 2
param$hbv.init.perc <- c(0.7, 0.6, 0.5)
param$prep.otc.hbv.flare.prob <- 0.75
param$prep.hbv.flare.prob <- 0.5
param$gfr.90.decline.rate <- c(0.001, 0.002)
param$gfr.60.decline.rate <- c(0.001, 0.002)
param$gfr.decline.prep.rate <- 10 * 3.8e-4 # 2 per 100 pyar
param$gfr.decline.age.gt50.or <- 6 * 5
param$gfr.decline.gfr.lt90.or <- 8.5
param$gfr.prep.recov.rate <- 0.1591036

i2r_p <- function(i, p) 1 - (1 - p)^(1 / i)
r2i_p <- function(r, p) log(1 - p, base = 1 - r)

r2i_p(0.07692308, 0.5)

# Epidemic simulation
dat_list <- netsim(est, param, init, control)
cat("\n")
dat <- dat_list[[1]]
sim <- process_out.net(dat_list)

# Simulation exploration (tidyverse)
d_sim <- as_tibble(sim)
glimpse(tail(d_sim, 15))

d_sim |>
  tail(15) |>
  select(starts_with("dbg_")) |>
  glimpse()

# GFR --------------------------------------------------------------------------

# GFR < 60
d_sim |>
  select(time, starts_with("dbg_gfr60_prop_")) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line()

# GFR < 90
d_sim |>
  select(time, starts_with("dbg_gfr90_prop_")) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line()

# GFR drop prep (per 100 pyar)
d_sim |>
  mutate(
    dgfr_yo50 = dbg_gfr_drop_yo50 / dbg_yo50_pwar * 52 * 100,
    dgfr_yo30 = dbg_gfr_drop_yo30 / dbg_yo30_pwar * 52 * 100,
    dgfr_gfr90 = dbg_gfr_drop_gfr90 / dbg_gfr90_pwar * 52 * 100,
    dgfr_gfr100 = dbg_gfr_drop_gfr100 / dbg_gfr100_pwar * 52 * 100,
  ) |>
  select(time, starts_with("dgfr_")) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  coord_cartesian(ylim = c(0, 1000)) + # restrict scale, keep all values
  geom_line() +
  geom_smooth()

# GFR recov - prop of elig to recov that actually recov
d_sim |>
  mutate(
    dgfr_recov = dbg_gfr_recov / dbg_gfr_recov_elig
  ) |>
  select(time, starts_with("dgfr_")) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# HBV:  ------------------------------------------------------------------------

# HBV: proportion
d_sim |>
  select(time, starts_with("dbg_hbv_prop_")) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line()

# flares numbers
d_sim |>
  select(time, starts_with("dbg_hbv_flares_")) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() + geom_smooth()

# flares prop among elig
d_sim |>
  mutate(
    dflare_std = dbg_hbv_flares_std / dbg_stop_std_prep,
    dflare_otc = dbg_hbv_flares_otc / dbg_stop_otc_prep,
  ) |>
  select(time, starts_with("dflare_")) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# Resistance  ------------------------------------------------------------------
d_sim |>
  select(time, starts_with(c("dbg_ftc"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line()

# Resistance  ------------------------------------------------------------------
d_sim |>
  select(time, starts_with(c("dbg_tdf"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line()

# PrEP OTC ---------------------------------------------------------------------
#
# OTC Episodes
d_sim |>
  select(time, starts_with(c("dbg_prep_otc_eps_"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# OTC any (episodes > 0)
d_sim |>
  select(time, dbg_prep_otc_any) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# OTC cur
d_sim |>
  select(time, dbg_prep_otc_cur, dbg_prep_otc_elig) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# OTC cov
d_sim |>
  mutate(otc_cov = dbg_prep_otc_cur / dbg_prep_otc_elig) |>
  select(time, otc_cov) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# OTC starts and stop
d_sim |>
  select(time, starts_with(c("dbg_prep_otc_st"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# OTC switch nums
d_sim |>
  select(time, starts_with(c("dbg_switch_"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# OTC switch probs
d_sim |>
  mutate(
    switch_otc_std = dbg_switch_otc_std / dbg_switch_otc_std_elig,
    switch_std_otc = dbg_switch_std_otc / dbg_switch_std_otc_elig
  ) |>
  select(time, starts_with(c("switch_"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# OTC stop due to GFR
d_sim |>
  select(time, starts_with(c("dbg_prep_otc_gfr"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# PrEP STD ---------------------------------------------------------------------
#
# STD Episodes
d_sim |>
  select(time, starts_with(c("dbg_prep_eps_"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# STD any (episodes > 0)
d_sim |>
  select(time, dbg_prep_any) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# STD cur
d_sim |>
  select(time, dbg_prep_cur, dbg_prep_elig) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# STD cov
d_sim |>
  mutate(std_cov = dbg_prep_cur / dbg_prep_otc_elig) |>
  select(time, std_cov) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# STD starts and stop
d_sim |>
  select(time, starts_with(c("dbg_prep_st"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# PrEP stop due to GFR
d_sim |>
  select(time, starts_with(c("dbg_prep_gfr"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

# sti tests --------------------------------------------------------------------
d_sim |>
  select(time, starts_with(c("dbg_hiv"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

d_sim |>
  select(time, starts_with(c("dbg_gono_"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

d_sim |>
  select(time, starts_with(c("dbg_chla_"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()

d_sim |>
  select(time, starts_with(c("dbg_syph_"))) |>
  pivot_longer(-time) |>
ggplot(aes(x = time, y = value, col = name)) +
  geom_line() +
  geom_smooth()
