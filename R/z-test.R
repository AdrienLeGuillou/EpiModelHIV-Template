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
library(ggplot2)
pkgload::load_all(EMHIVp_dir)
# library(EpiModelHIV)

context <- "hpc"
source("R/netsim_settings.R", local = TRUE)

d_calib <- readRDS(fs::path(scenarios_dir, "merged_tibbles", "df__empty_scenario.rds"))
source("./R/F-intervention_scenarios/outcomes.R", local = TRUE)
d_table <- mutate_outcomes(d_calib) |>
  mutate(scenario_name = "empty")
source("R/F-intervention_scenarios/labels.R", local = TRUE)
format_table(d_table, var_labels, format_patterns) |>
  as.list()


d_calib |> tail(10 * year_steps) |> pull(dbg_hbv_flares_std) |> sum()
d_calib |> tail(10 * year_steps) |> pull(dbg_hbv_flares_otc) |> sum()

d_table$cml_hbv_flare




targets <- EpiModelHIV::get_calibration_targets()

d_outs <- EpiModelHIV::mutate_calibration_targets(d_calib) |>
  mutate(sim = as.integer(as.factor(paste0(batch_number, "_", sim)))) |>
  select(sim, time, any_of(names(targets))) |>
  as.epi.data.frame()



targets <- EpiModelHIV::get_calibration_targets()
races_names <- c("B", "H", "W")
races <- 1:3

med_iqr <- function(x, fmtr) {
  vs <- quantile(x, c(0.5, 0.25, 0.75)) |> fmtr()
  paste0(vs[1], " [", vs[2], "-",  vs[3], "]")
}

p <- calib_plot_infos[["gfr_60"]]
make_calib_plot(d_outs, p)



# Observed data
x_vals <- c(100, 120, 130)
p_obs  <- c(0.0027, 0.02, 0.084)

# Define the loss function to minimize
loss_function <- function(params) {
  mu <- params[1]
  sigma <- params[2]
  # Predicted cumulative probabilities under N(mu, sigma)
  p_pred <- pnorm(x_vals, mean = mu, sd = sigma)
  # Sum of squared differences
  sum((p_pred - p_obs)^2)
}

# Initial guesses for mu and sigma
init_params <- c(mu = 140, sigma = 10)

# Run the optimizer
result <- optim(par = init_params, fn = loss_function, method = "L-BFGS-B",
                lower = c(-Inf, 1e-6))  # Ensure sigma > 0

# Extract results
mu_est <- result$par[1]
sigma_est <- result$par[2]

# Print results
cat("Optimized mean (mu):", mu_est, "\n")
cat("Optimized standard deviation (sigma):", sigma_est, "\n")

x <- rnorm(1e4, mu_est, sigma_est)
mean(x < 100)
mean(x < 120)
mean(x < 130)




# `param` is the original parameter list, p_env is the environment version
p_env <- as.environment(param)

# Accessing the first parameter
elt_name <- names(param)[1]
microbenchmark::microbenchmark(times = 1000L,
  param[[elt_name]],
  p_env[[elt_name]]
)
#> Unit: nanoseconds
#>               expr min  lq    mean median    uq   max neval
#>  param[[elt_name]] 688 742 821.120    769 808.5 14082  1000
#>  p_env[[elt_name]] 145 165 196.148    189 204.0  3210  1000

elt_name <- names(param)[1]
bench::mark(iterations = 143,
  param[[elt_name]],
  p_env[[elt_name]]
)

# Accessing the last parameter (117)
elt_name <- names(param)[length(param)]
microbenchmark::microbenchmark(times = 1000L,
  param[[elt_name]],
  p_env[[elt_name]]
)
#> Unit: nanoseconds
#>               expr  min   lq     mean median   uq   max neval
#>  param[[elt_name]] 2288 2377 2513.996   2411 2466 22326  1000
#>  p_env[[elt_name]]  143  168  200.902    188  217  3769  1000

elt_name <- names(param)[length(param)]
bench::mark(iterations = 143,
  param[[elt_name]],
  p_env[[elt_name]]
) |> select(-expression)

p_env$vital <- TRUE

lapply(p_env, length)
