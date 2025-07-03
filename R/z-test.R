# Scratchpad for interactive testing before integration in a script

# rmarkdown::render(
#   "R/Z-calibration/calibration_values.Rmd",
#   output_file = "calibration_report.html",
#   knit_root_dir = getwd(),
#   output_dir = "./"
# )

source("R/shared_variables.R", local = TRUE)

library(dplyr)
library(tidyr)

sim <- readRDS("./data/run/scenarios/sim__only_otc_same_1__1.rds")
sim <- readRDS("./data/run/scenarios/sim__only_otc_free_1__1.rds")
d <- as_tibble(sim$run$sim1$attr)

d |>
  filter(
    prep.otc == 1,
    gfr < 60
  )

d <- as_tibble(sim)
d$dbg_prep_otc_gfr_stop |> tail(50)
d$prep_any_gfr_lt60 |> tail(50)


# mean formulation
r2i_mean <- function(r) 1 / r
i2r_mean <- function(i) 1 / i

# median formulation
r2i_med <- function(r) -1 / (log2(1 - r))
i2r_med <- function(i) 1 - 0.5^(1 / i)

# quantile - prob (p) that event occurs after interval (i)
i2r_p <- function(i, p) 1 - (1 - p)^(1 / i)
r2i_p <- function(r, p) log(1 - p, base = 1 - r)

p_start = 0.0
disc_int = 33.42
p_stop = i2r_med(disc_int) # given
prev = 0.2 # target
# p_start = p_stop * n_prep / (n_elig + n_stop)
# n_prep / (n_elig + n_stop) = prev / (prev * p_stop + (1 - prev))
# prep_odd = prev / ((1 - prev)) # simpler formulation
prep_odd = prev / (prev * p_stop + (1 - prev))
prep_start = p_stop * prep_odd
prep_start
