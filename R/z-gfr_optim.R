# This code fits a normal distribution for GFR at birth assuming linear decline
# and the know values for <60
# Observed data
x_vals <- 60 + c(40, 60, 70) #
p_obs  <- c(0.0027, 0.02, 0.084)  # raw vals per group
p_obs  <- cumsum(p_obs) # vals below age X

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
init_params <- c(mu = 160, sigma = 20)

# Run the optimizer
result <- optim(par = init_params, fn = loss_function, method = "L-BFGS-B",
  lower = c(-Inf, 1e-6))  # Ensure sigma > 0

# Extract results
mu_est <- result$par[1]
sigma_est <- result$par[2]

# Print results
cat("Optimized mean (mu):", mu_est, "\n") # 146.4956
cat("Optimized standard deviation (sigma):", sigma_est, "\n") # 13.25594

x <- rnorm(1e5, mu_est, sigma_est)
mean(x < 60 + 40) # prop of < 60 and 40yo
mean(x < 60 + 60)
mean(x < 60 + 70)

library(ggplot2)

ggplot(data.frame(x = x), aes(x = x)) +
  geom_density() +
  geom_vline(xintercept = 60 + 40) +
  annotate("label", x = 60 + 40, y = 0.02, label = "40yo") +
  annotate("label", x = 60 + 40, y = 0.015, label = mean(x < 60 + 40)) +
  geom_vline(xintercept = 60 + 60) +
  annotate("label", x = 60 + 60, y = 0.02, label = "60yo") +
  annotate("label", x = 60 + 60, y = 0.015, label = mean(x < 60 + 60)) +
  geom_vline(xintercept = 60 + 70) +
  annotate("label", x = 60 + 70, y = 0.02, label = "70yo") +
  annotate("label", x = 60 + 70, y = 0.015, label = mean(x < 60 + 70))


ggplot(data.frame(x = x - 19), aes(x = x)) +
  geom_density()

summary(x-19)

mean((x - 19 - 33) < 60)
mean((x - 24 - 31) < 60)
mean((x - 29 - 28) < 60)
mean((x - 35 - 24.7) < 60)
mean((x - 45 - 19) < 60)
mean((x - 55 - 12) < 60)

library(dplyr)

mu_est <- 145
sigma_est <- 20
n <- 1e5
x <- rnorm(n, mu_est, sigma_est)
age_breaks <- c(15, 20, 25, 30, 40, 50, 66)
hr_age <- c(1, 1.28, 1.57, 1.63, 2.65, 6.05)
# hr_age <- rep(1, length(age_breaks))
gfr_breaks <- c(0, 60, 90, 200)
hr_gfr <- c(100, 8.34, 1)
# hr_gfr <- c(1, 1, 1)

d_sample <- tibble(
  age = sample(15:65, n, replace = T),
  age_grps = cut(age, age_breaks, right = FALSE, label = FALSE),
  raw_gfr = x,
  gfr = raw_gfr - age,
  gfr_grps = cut(gfr, gfr_breaks, right = FALSE, label = FALSE),
  prep_gfr = F,
)

d_sample |>
  group_by(age_grps) |>
  summarise(
    n = n(),
    gfr_gt90 = mean(gfr_grps == 3) * 100,
    gfr_lt90 = mean(gfr_grps == 2) * 100,
    gfr_lt60 = mean(gfr_grps == 1) * 100
  )


base_prob <- 0.00004
d_exp <- d_sample |>
  mutate(
    prep_gfr = runif(n) < base_prob * hr_age[age_grps] * hr_gfr[gfr_grps],
  )
# p_gfr <- c(1, 0.0064, 0.0834)
#
# d_exp <- d_sample |>
#   mutate(
#     prep_gfr = runif(n) < p_gfr[gfr_grps],
#   )
d_exp |>
  group_by(age_grps) |>
  summarise(ir100 = sum(prep_gfr) / n() * 52 * 100)
d_exp |>
  group_by(gfr_grps) |>
  summarise(ir100 = sum(prep_gfr) / n() * 52 * 100)


# quantile - prob (p) that event occurs after interval (i)
i2r_p <- function(i, p) 1 - (1 - p)^(1 / i)
r2i_p <- function(r, p) log(1 - p, base = 1 - r)

i2r_p(52, 0.08)
i2r_p(2 * 52, 0.13)
i2r_p(3 * 52, 0.25)

0.64 / 100






#### With data from DOI: 10.1016/S2352-3018(22)00004-2

library(dplyr)
library(ggplot2)

d_gfr <- dplyr::tribble(
  ~age_grp, ~n, ~gfr_ge90, ~gfr_lt90, ~gfr_lt60, ~max_age, ~duration,
  "15-19", 1156, 95.3, 4.58, 0.09, 19, 5,
  "20-24", 3631, 88.5, 11.5, 0.03, 24, 5,
  "25-29", 4253, 83.5, 16.3, 0.21, 29, 5,
  "30-39", 5751, 74.8, 24.8, 0.37, 39, 10,
  "40-49", 2584, 64.6, 34.7, 0.70, 49, 10,
  "50+", 1254, 48.5, 49.7, 1.83, 60, 10
)

d_gfr <- d_gfr |>
  mutate(
    n_gfr_ge90 = round(gfr_ge90 * n / 100),
    n_gfr_lt90 = round(gfr_lt90 * n / 100),
    n_gfr_lt60 = round(gfr_lt60 * n / 100)
  )

glimpse(d_gfr)


# Gi: already have G (GFR <90)
# Gt: target coverage of G
# Gs: susceptible to G (1 - ini)
# n: number of steps
# P: per step prob of G
#
# Gt = Gi + Gs * (1 - (1-P)^n)
# P = 1 - (1 - (Gt - Gi) / Gs)^(1 / n)

get_p <- function(tar, ini, steps) {
  susc <- 1 - ini
  1 - (1 - (tar - ini) / susc)^(1 / steps)
}

get_p(0.05, 0.02, 52 * 10)

d_c <- d_gfr |>
  select(max_age, duration, starts_with("n")) |>
  mutate(regroup = c(1, 1, 1, 2, 2, 3))
  # mutate(regroup = c(1, 1, 1, 3, 4, 5))

d_c <- d_c |>
  group_by(regroup) |>
  summarise(
    max_age = max(max_age),
    duration = sum(duration),
    n = sum(n),
    n_gfr_ge90 = sum(n_gfr_ge90),
    n_gfr_lt90 = sum(n_gfr_lt90),
    n_gfr_lt60 = sum(n_gfr_lt60),
    gfr_ge90 = n_gfr_ge90 / n ,
    gfr_lt90 = n_gfr_lt90 / n,
    gfr_lt60 = n_gfr_lt60 / n
  )

select(d_c, max_age, starts_with("gfr"))

d_calc90 <- d_c |>
  mutate(
    tar = (n - n_gfr_ge90) / n,
    # tar = n_gfr_lt60 / n,
    ini = lag(tar, default = 0),
    steps = duration * 52
  ) |>
  select(n, max_age, tar, ini, steps)

d_calc90

Map(get_p, d_calc90$tar, d_calc90$ini, d_calc90$steps)


################################################################################
################################################################################
###
### The one I actually used
###
################################################################################
################################################################################

n_nodes <- 1e4
n_steps <- 46 * 52
gfr.90.decline.rate <- c(2.2e-4, 2.2e-4, 5.0e-4)
gfr.60.decline.rate <- c(3e-5, 3e-5, 8e-5)
# gfr.60.decline.rate <- c(1.56e-6, 4.69e-6, 6.41e-6, 22.16e-6)

# Init -------------------------------------------------------------------------
gfr <- rep(100, n_nodes)
age <- sample(15:65, n_nodes, replace = TRUE)

# Loop -------------------------------------------------------------------------
for (at in seq_len(n_steps)) {
  # aging
  age <- age + 1 / 52
  # departure / arrival
  age_out_ids <- which(age > 65)
  age[age_out_ids] <- 15
  gfr[age_out_ids] <- sample(
    c(100, 75),
    length(age_out_ids), prob = c(0.97, 0.03),
    replace = TRUE
  )

  # gfr decline

  age_grps <- cut(age, c(15, 30, 50, 65), labels = FALSE, right = FALSE)

  # Individual 75 becoming < 60
  elig_ids <- which(gfr == 75)
  rates <- gfr.60.decline.rate[age_grps[elig_ids]]
  decline_60_ids <- elig_ids[runif(length(elig_ids)) < rates]
  gfr[decline_60_ids] <- 50

  # Individual > 90 becoming < 90 (75)
  elig_ids <- which(gfr == 100)
  rates <- gfr.90.decline.rate[age_grps[elig_ids]]
  decline_90_ids <- elig_ids[runif(length(elig_ids)) < rates]
  gfr[decline_90_ids] <- 75
}

# Calc epi ---------------------------------------------------------------------
# select(d_c, max_age, starts_with("gfr"))
tapply(gfr, age_grps, \(x) mean(x < 90))
tapply(gfr, age_grps, \(x) mean(x < 60))

# Recovery after prep: 75% recov in 8 weeks
#
# quantile - prob (p) that event occurs after interval (i)
i2r_p <- function(i, p) 1 - (1 - p)^(1 / i)
r2i_p <- function(r, p) log(1 - p, base = 1 - r)

i2r_p(8, 0.75)
