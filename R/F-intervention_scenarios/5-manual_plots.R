library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_light())

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/outcomes.R", local = TRUE)

# Process ----------------------------------------------------------------------

d_best <-
  readRDS("./data/run/scenarios/merged_tibbles/df__otc_best_adhr_base.rds") |>
  select(time, prep_cli = prepCurr, prep_otc = prep.otcCurr) |>
  mutate(prep_any = prep_cli + prep_otc, group = "best")

d_mix <-
  readRDS("./data/run/scenarios/merged_tibbles/df__otc_mix_adhr_base.rds") |>
  select(time, prep_cli = prepCurr, prep_otc = prep.otcCurr) |>
  mutate(prep_any = prep_cli + prep_otc, group = "mix")

d <- bind_rows(d_best, d_mix) |>
  pivot_longer(-c(time, group)) |> group_by(time, group, name) |>
  summarise(
    q1 = quantile(value, 0.025),
    q2 = quantile(value, 0.5),
    q3 = quantile(value, 0.975)
  )

ggplot(d, aes(x = time / 52, y = q2, ymin = q1, ymax = q3, col = name, fill = name)) +
  geom_ribbon(alpha = 0.3, linewidth = 0) +
  geom_line() +
  facet_grid(~group)

ggsave("./data/output/mix plots.png")
