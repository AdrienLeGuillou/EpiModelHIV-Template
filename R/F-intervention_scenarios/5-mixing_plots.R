library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_light())

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/outcomes.R", local = TRUE)

# Process ----------------------------------------------------------------------

add_otc_files <- c(
  # p10 = "./data/run/scenarios/merged_tibbles/df__add_otc_best0.06595926.rds",
  p20 = "./data/run/scenarios/merged_tibbles/df__add_otc_best0.14680892.rds",
  # p30 = "./data/run/scenarios/merged_tibbles/df__add_otc_best0.25019997.rds",
  p40 = "./data/run/scenarios/merged_tibbles/df__add_otc_best0.38801056.rds"
  # p50 = "./data/run/scenarios/merged_tibbles/df__add_otc_best0.57211886.rds"
)

d_ls <- lapply(seq_along(add_otc_files), \(i) {
  readRDS(add_otc_files[i]) |>
    select(time, prep_cli = prepCurr, prep_otc = prep.otcCurr) |>
    mutate(prep_any = prep_cli + prep_otc, group = names(add_otc_files[i]))
})


d <- bind_rows(d_ls) |>
  # d <- bind_rows(d_best, d_mix) |>
  pivot_longer(-c(time, group)) |>
  group_by(time, group, name) |>
  summarise(
    q1 = quantile(value, 0.025),
    q2 = quantile(value, 0.5),
    q3 = quantile(value, 0.975)
  )

ggplot(
  d,
  aes(x = time / 52, y = q2, ymin = q1, ymax = q3, col = name, fill = name)
) +
  geom_ribbon(alpha = 0.3, linewidth = 0) +
  geom_line() +
  geom_hline(yintercept = 12658, linetype = 2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(~group) +
  labs(
    x = "Time (years)",
    y = "Number on PrEP",
    col = "Type of PrEP",
    fill = "Type of PrEP"
  ) +
  theme(
    axis.title = element_text(size = 17, face = "bold"),
    axis.text = element_text(size = 17),
    strip.text = element_text(size = 17),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 17)
  )

ggsave("./data/output/mix plots.png")