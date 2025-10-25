## 3. swfcalib Assessment
##
## interactive script to evaluate why an swfcalib process did not returned the
## expected results. It creates the assessment report and interactively look
## into the `results.rds` object found in the calibration folder.

# Restart R before running this script (Ctrl_Shift_F10 / Cmd_Shift_0)

# Setup ------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_light())

source("R/shared_variables.R", local = TRUE)
source("R/Z-calibration/z-context.R", local = TRUE)

# Process ----------------------------------------------------------------------

theme_set(theme_light())

# SWFCalib Assessment ----------------------------------------------------------
source("R/shared_variables.R", local = TRUE)
swfcalib::render_assessment(fs::path(swfcalib_dir, "assessments.rds"))

# Finalized calibration assessment  --------------------------------------------
rmarkdown::render(
  "R/Z-calibration/calibration_values.Rmd",
  output_file = "calibration_report.html",
  knit_root_dir = getwd(),
  output_dir = "./"
)

# Results ----------------------------------------------------------------------
targets <- EpiModelHIV::get_calibration_targets()
results <- readRDS(fs::path(swfcalib_dir, "results.rds"))
# readr::write_csv(results, "../tst_sk_calib/res_syph.csv")

ggplot(
  results,
  aes(x = hiv.test.rate_1, y = cc.dx.B, col = as.factor(.iteration))
) +
  geom_point() +
  geom_hline(yintercept = targets[["cc.dx.B"]])

n_it <- 10

max_zero <- results |>
  filter(.iteration <= n_it) |>
  filter(ir100.gono == 0) |>
  pull(gono.uret.prob) |>
  max()

results |>
  filter(ir100.gono > max_zero) |>
  ggplot(aes(x = gono.uret.prob, y = ir100.gono, col = as.factor(.iteration))) +
  geom_point() +
  geom_hline(yintercept = targets[["ir100.gono"]])

dlm <- results |>
  filter(ir100.gono > max_zero) |>
  filter(.iteration <= n_it) #|> sample_n(100)

d_mod <- tibble(
  x = dlm[["gono.uret.prob"]],
  y = dlm[["ir100.gono"]]
)
mod <- lm(y ~ poly(x, 1), data = d_mod)
summary(mod)

loss_fun <- function(par, t) abs(predict(mod, data.frame(x = par)) - t)
optimize(interval = c(0.1, 0.5), f = loss_fun, t = targets["ir100.gono"])

mutate(
  d_mod,
  pred = predict(mod)
) |>
  ggplot(aes(
    x = x,
    y = y
  )) +
  geom_point() +
  geom_hline(yintercept = targets["ir100.gono"]) +
  # geom_smooth() +
  geom_line(aes(y = pred)) +
  xlim(0.1, 0.25)


dlm <- filter(results, .iteration <= 1) #|> sample_n(100)
p_name <- "tx.halt.partial.rate_"
t_name <- "cc.vsupp."
d_mod <- tibble(
  x = unname(unlist(dlm[paste0(p_name, 1:3)])),
  y = unname(unlist(dlm[paste0(t_name, c("B", "H", "W")[1:3])]))
  # x = dlm[[p_name]],
  # y = dlm[[t_name]]
)
mod <- lm(y ~ poly(x, 3), data = d_mod)
summary(mod)

loss_fun <- function(par, t) abs(predict(mod, data.frame(x = par)) - t)
optimize(
  interval = c(0.001, 0.01),
  f = loss_fun,
  t = targets[paste0(t_name, "H")]
)

mutate(
  d_mod,
  pred = predict(mod)
) |>
  ggplot(aes(
    x = x,
    y = y
  )) +
  geom_point() +
  # geom_hline(yintercept = targets[t_name]) +
  # geom_hline(yintercept = targets["cc.linked1m.H"]) +
  # geom_hline(yintercept = targets["cc.linked1m.W"]) +
  # geom_smooth() +
  geom_line(aes(y = pred))

# range at each iteration
results |>
  group_by(.iteration) |>
  summarize(
    lo = min(gono.uret.prob),
    med = median(gono.uret.prob),
    hi = max(gono.uret.prob)
  )

results |>
  filter(abs(ir100.gono - targets[t_name]) < 1) |>
  select(gono.uret.prob, ir100.gono) |>
  pull(gono.uret.prob) |>
  median()


# quantile

i2r_p <- function(i, p) 1 - (1 - p)^(1 / i)
r2i_p <- function(r, p) log(1 - p, base = 1 - r)
i2r_p(52 / 12, targets[paste0(t_name, "B")])
i2r_p(52 / 12, targets[paste0(t_name, "H")])
i2r_p(52 / 12, targets[paste0(t_name, "W")])

i2r_p(52 / 12, 0.8)

p = 0.8
l = kkk

i2r_p(52 / 12, 0.898)
r2i_p(i2r_p(4, 0.8), 0.8)


dlm <- filter(results, .iteration <= 1) |>
  select(prep.start.rate_1, cc.prep.B)

mod <- lm(cc.prep.B ~ poly(prep.start.rate_1, 2, raw = F), data = dlm)
summary(mod)

mod <- lm(cc.prep.B ~ prep.start.rate_1 + prep.start.rate_1^2, data = dlm)
summary(mod)


pu <- results |>
  filter(abs(ir100.gc - 12.81) < 0.1) |>
  pull(ugc.prob) |>
  median()

pgc <- results |>
  filter(abs(ugc.prob - 0.2584717) < 0.001) |>
  select(ugc.prob, ir100.gc)

results |>
  filter(.iteration == max(.iteration)) |>
  pull(hiv.test.rate_1) |>
  range()

filter(results, .iteration > 1) |>
  ggplot(aes(
    x = ugc.prob,
    y = ir100.gc,
    col = as.factor(.iteration)
  )) +
  geom_point() +
  geom_hline(yintercept = 12.81) +
  geom_vline(xintercept = 0.25867) +
  geom_smooth()

filter(results, .iteration > 1) |>
  ggplot(aes(
    x = uct.prob,
    y = ir100.ct,
    col = as.factor(.iteration)
  )) +
  geom_point() +
  geom_hline(yintercept = 14.59) +
  geom_vline(xintercept = 0.1833) +
  geom_smooth()


ggplot(
  results,
  aes(
    x = hiv.test.rate_1,
    y = cc.dx.B,
    col = as.factor(.iteration)
  )
) +
  geom_point() +
  geom_hline(yintercept = 0.847) +
  geom_vline(xintercept = 0.002688045)


# range at each iteration
results |>
  group_by(.iteration) |>
  summarize(
    lo = min(a.rate),
    med = median(a.rate),
    hi = max(a.rate)
  )


results |>
  select(starts_with("hiv.trans"), starts_with("i.prev.dx")) |>
  mutate(
    i.prev.dx.B = i.prev.dx.B - 0.33,
    i.prev.dx.H = i.prev.dx.H - 0.127,
    i.prev.dx.W = i.prev.dx.W - 0.09,
    se = i.prev.dx.B^2 + i.prev.dx.H^2 + i.prev.dx.W^2,
    B = abs(i.prev.dx.B),
    H = abs(i.prev.dx.H),
    W = abs(i.prev.dx.W),
  ) |>
  select(se, everything()) |>
  arrange(se) |>
  filter(B < 0.02, H < 0.02, W < 0.01) |>
  summarise(across(starts_with("hiv.trans"), median)) |>
  as.list()

co <- readRDS("./calib_object.rds")


results |>
  filter(ir100.gc > 0) |>
  ggplot(aes(
    x = ugc.prob,
    y = ir100.gc,
    col = as.factor(.iteration)
  )) +
  geom_point() +
  geom_hline(yintercept = 12.81) +
  geom_smooth()


r0 <- results |> filter(ir100.gc > 0)

mod <- lm(ir100.gc ~ ugc.prob, data = r0)

plot(mod)


loss_fun <- function(par, t) abs(predict(mod, data.frame(ugc.prob = par)) - t)
optimize(interval = c(0.24, 0.3), f = loss_fun, t = 12.81)
