# Scratchpad for interactive testing before integration in a script

# rmarkdown::render(
#   "R/Z-calibration/calibration_values.Rmd",
#   output_file = "calibration_report.html",
#   knit_root_dir = getwd(),
#   output_dir = "./"
# )

rmarkdown::render(
  "./Rmd/scenarios_explore.Rmd",
  output_file = "scenarios_explore.html",
  knit_root_dir = getwd(),
  output_dir = "./"
)

source("R/shared_variables.R", local = TRUE)

library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_light())

sim <- readRDS("./data/run/scenarios/sim__otc_best_guess__1.rds")
age.breaks <- sim$param$netstats$demog$age.breaks
age_grp_names <- cut(age.breaks[-1], age.breaks) |>
  levels()
attr <- sim$run[[1]]$attr
prep_std <- attr$prep
prep_otc <- attr$prep.otc
age_grp <- attr$age.grp

table(age_grp, prep_otc) |>
  prop.table()

prop <- table(age_grp[prep_otc == 1]) |>
  prop.table() |>
  (\(x) paste0(round(x * 100, 1), "%"))()
names(prop) <- age_grp_names
prop

prop <- table(age_grp[prep_std == 1]) |>
  prop.table() |>
  (\(x) paste0(round(x * 100, 1), "%"))()
names(prop) <- age_grp_names
prop

b = 12645
p = c(1.15, 1.3, 1.4, 1.5)
b * p
x = c(14621, 16247, 17692, 18940)
x / b - 1


# Pot PrEP vs OTC --------------------------------------------------------------
source("R/shared_variables.R", local = TRUE)

library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_light())
scs <- c(
  "only_otc_best",
  "only_otc_relaxed",
  "otc_best",
  "otc_mix",
  "only_otc_same"
)
sc <- scs[3]
d <- readRDS(paste0(scenarios_dir, "merged_tibbles/df__", sc, "_adhr_base.rds"))

d |>
  select(sim, time, prepCurr, prep.otcCurr) |>
  mutate(prep_any = prepCurr + prep.otcCurr) |>
  pivot_longer(-c(sim, time)) |>
  ggplot(aes(x = time / 52, y = value, col = name)) +
  geom_smooth()

ggsave("data/output/preps_best.png")
