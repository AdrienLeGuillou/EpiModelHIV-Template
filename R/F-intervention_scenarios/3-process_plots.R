## 3. Intervention Scenarios Process Plots
##
## Make the plots using the results of the simulations from the previous step
## locally or on the HPC (see `workflow-interventions.R`)

# Restart R before running this script (Ctrl_Shift_F10 / Cmd_Shift_0)

# Setup ------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_light())
library(viridis)

source("R/shared_variables.R", local = TRUE)
source("./R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/outcomes.R", local = TRUE)

# Process ----------------------------------------------------------------------

d_cont <- readRDS("./data/run/scenarios/plots/df_cont_plot.Rds") |>
  group_by(prop_otc, tst_rate) |>
  summarise(across(everything(), median)) |>
  mutate(cml_resist_inf = cml_resist / (1e4 - cml_nia_all))
glimpse(d_cont)

# resist Plots -----------------------------------------------------------------
# TODO: CML_RESIST per cuml infs - rougly 10k inf in baseline
loess_mod <- loess(
  cml_resist_inf ~ prop_otc * tst_rate,
  data = d_cont,
  span = 0.25
)
loess_inter <- expand.grid(list(
  prop_otc = seq(0, 1, 0.01),
  tst_rate = seq(0, 1, 0.01)
))
loess_inter$z <- as.numeric(predict(loess_mod, newdata = loess_inter)) * 100
loess_inter$x <- loess_inter$prop_otc
loess_inter$y <- loess_inter$tst_rate

p_res <- ggplot(loess_inter, aes(x, y)) +
  geom_raster(aes(fill = z), interpolate = TRUE) +
  geom_contour(aes(z = z), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_classic() +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(0, 1, 0.25),
    labels = scales::label_percent(1)(seq(0, 1, 0.25))
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 1, 0.25),
    labels = scales::label_percent(1)(seq(0, 1, 0.25))
  ) +
  labs(
    y = "Probability of HIV test before OTC start",
    x = "Proportion of OTC PrEP"
  ) +
  scale_fill_viridis(
    "Resistances \nper 100 Infections",
    discrete = FALSE,
    alpha = 1,
    option = "A",
    direction = 1
    # breaks = seq(0, 0.3, 0.1),
    # labels = scales::label_percent(1)(seq(0, 0.3, 0.1))
  ) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks = element_line(color = "black")
  ) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = 2) +
  geom_vline(xintercept = 0.3, color = "gray", linetype = 2) +
  coord_fixed(ratio = 1)


# PIA Plot ---------------------------------------------------------------------
loess_mod <- loess(
  cml_pia_all ~ prop_otc * tst_rate,
  data = d_cont,
  span = 0.25
)
loess_inter <- expand.grid(list(
  prop_otc = seq(0, 1, 0.01),
  tst_rate = seq(0, 1, 0.01)
))
loess_inter$z <- as.numeric(predict(loess_mod, newdata = loess_inter))
loess_inter$x <- loess_inter$prop_otc
loess_inter$y <- loess_inter$tst_rate

p_pia <- ggplot(loess_inter, aes(x, y)) +
  geom_raster(aes(fill = z), interpolate = TRUE) +
  geom_contour(aes(z = z), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_classic() +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(0, 1, 0.25),
    labels = scales::label_percent(1)(seq(0, 1, 0.25))
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 1, 0.25),
    labels = scales::label_percent(1)(seq(0, 1, 0.25))
  ) +
  labs(
    y = "Probability of HIV test before OTC start",
    x = "Proportion of OTC PrEP"
  ) +
  scale_fill_viridis(
    "Percent of \nInfections Averted",
    discrete = FALSE,
    alpha = 1,
    option = "A",
    direction = 1,
    breaks = seq(0, 0.2, 0.05),
    labels = scales::label_percent(0.1)(seq(0, 0.2, 0.05))
  ) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks = element_line(color = "black")
  ) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = 2) +
  geom_vline(xintercept = 0.3, color = "gray", linetype = 2) +
  coord_fixed(ratio = 1)

p_comb <- gridExtra::grid.arrange(p_pia, p_res, ncol = 2)
ggsave("data/output/contour.png", plot = p_comb, width = 14, height = 7)
