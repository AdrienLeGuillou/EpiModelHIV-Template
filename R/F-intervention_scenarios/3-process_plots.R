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
  summarise(across(everything(), median))
glimpse(d_cont)

ggplot(d_cont, aes(x = prop_otc, y = tst_rate, col = log(cml_resist))) +
  geom_point(size = 1) +
  scale_color_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1)

# loess_mod <- loess(log10(cml_resist) ~ prop_otc * tst_rate, data = d_cont, span = 0.25)
# loess_mod <- loess(cml_resist ~ prop_otc * tst_rate, data = d_cont, span = 0.25)
loess_mod <- loess(cml_addi_resist_nia ~ prop_otc * tst_rate, data = d_cont, span = 0.75)
# loess_mod <- loess(cml_pia_all ~ prop_otc * tst_rate, data = d_cont, span = 0.25)
loess_inter <- expand.grid(list(
  prop_otc = seq(0, 1, 0.01),
  tst_rate = seq(0, 1, 0.01)
))
loess_inter$z <- as.numeric(predict(loess_mod, newdata = loess_inter))
loess_inter$x <- loess_inter$prop_otc
loess_inter$y <- loess_inter$tst_rate

loess_inter$z <- ifelse(loess_inter$z < 0, NA, loess_inter$z)

ggplot(loess_inter, aes(x, y)) +
  geom_raster(aes(fill = z), interpolate = TRUE) +
  geom_contour(aes(z = z), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_classic() +
  # scale_y_continuous(expand = c(0, 0)) +
  # scale_x_continuous(expand = c(0, 0)) +
  labs(
    y = "Probability of HIV test before OTC start",
    x = "Proportion of OTC PrEP"
  ) +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks = element_line(color = "black")
  )
