# Scratchpad for interactive testing before integration in a script
rmarkdown::render(
  "R/Z-calibration/calibration_values.Rmd",
  output_file = "calibration_report.html",
  knit_root_dir = getwd(),
  output_dir = "./"
)

source("R/shared_variables.R", local = TRUE)

library(dplyr)
library(EpiModelHIV)

context <- "hpc"
source("R/netsim_settings.R", local = TRUE)

d_calib <- readRDS(fs::path(calib_dir, "merged_tibbles", "df__empty_scenario.rds"))
targets <- EpiModelHIV::get_calibration_targets()

d_outs <- EpiModelHIV::mutate_calibration_targets(d_calib) |>
  mutate(sim = as.integer(as.factor(paste0(batch_number, "_", sim)))) |>
  select(sim, time, any_of(names(targets))) |>
  as.epi.data.frame()

races <- c("B", "H", "W")
calib_plot_infos <- list(
  cc.dx = list(
    names = paste0("cc.dx.", races),
    ylab = "Proportion",
    text_offset = 0.01,
    fmt_target = scales::percent_format(0.1)
  ),
  cc.linked1m = list(
    names = paste0("cc.linked1m.", races),
    ylab = "Proportion",
    text_offset = 0.005,
    fmt_target = scales::percent_format(0.1)
  ),
  cc.vsupp = list(
    names = paste0("cc.vsupp.", races),
    ylab = "Proportion",
    text_offset = 0.005,
    fmt_target = scales::percent_format(0.1)
  ),
  i.prev.dx = list(
    names = paste0("i.prev.dx.", races),
    ylab = "Proportion",
    text_offset = 0.01,
    fmt_target = scales::percent_format(0.1)
  ),
  ir100.sti = list(
    names = c("ir100.gono", "ir100.chla", "ir100.syph"),
    ylab = "Infection Rate per 100 PYAR",
    text_offset = 0.3,
    fmt_target = scales::number_format(0.1)
  ),
  cc.prep = list(
    names = paste0("cc.prep.", races),
    ylab = "Proportion",
    text_offset = 0.005,
    fmt_target = scales::percent_format(0.1)
  ),
  disease.mr100 = list(
    names = "disease.mr100",
    ylab = "Proportion",
    text_offset = 0.01,
    fmt_target = scales::percent_format(0.1)
  ),
  num = list(
    names = "num",
    ylab = "Population",
    text_offset = 500,
    fmt_target = scales::number_format(1)
  )
)


make_calib_plot <- function(d, plot_info) {
  targets <- EpiModelHIV::get_calibration_targets()
  targets["num"] <- 1e5
  colors <-  c("steelblue", "firebrick", "seagreen")
  text_pos <- max(d$time) - 500
  par(mar = c(3, 3, 1, 1), mgp = c(2, 1, 0))
  offset <- plot_info$text_offset
  cur_targs <- plot_info$names

  pkgload::load_all("../EpiModel.git/main/")

  plot(
    d,
    xaxt = "none",
    y = cur_targs,
    legend = TRUE,
    ylab = plot_info$ylab,
    xlab = "Calibration Years"
  )
  axis(1, seq(0, max(d$time), 10 * year_steps),
       labels = seq(0, max(d$time), 10 * year_steps) / year_steps)

  x <- round(colMeans(tail(d_outs[, cur_targs], 52)), 3)
  abline(h = targets[cur_targs], col = colors, lty = 2)
  for (i in seq_along(plot_info$names)) {
    v <- plot_info$fmt_target(x[i])
    text(text_pos, targets[cur_targs[i]] + offset, v, col = colors[i])
  }
}

targets <- EpiModelHIV::get_calibration_targets()
races_names <- c("B", "H", "W")
races <- 1:3

med_iqr <- function(x, fmtr) {
  vs <- quantile(x, c(0.5, 0.25, 0.75)) |> fmtr()
  paste0(vs[1], " [", vs[2], "-",  vs[3], "]")
}

p <- calib_plot_infos[["disease.mr100"]]
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
