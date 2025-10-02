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
library(akima)
library(viridis)

source("R/shared_variables.R", local = TRUE)
source("./R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/outcomes.R", local = TRUE)

# Process ----------------------------------------------------------------------

d_cont <- readRDS("./data/run/scenarios/plots/df_cont_plot.Rds")

d_cont |>
  filter(tst_rate == 0) |>
ggplot(aes(x = prop_otc, y = cml_resist)) +
  geom_line()

glimpse(d_cont)

prev.loess <- loess(cml_resist ~ prop_otc * tst_rate, data = d_cont)
prev.fit2 <- expand.grid(list(prop_otc = seq(0, 1, 0.01),
                              tst_rate = seq(0, 1, 0.01)))
prev.fit2$resist <- as.numeric(predict(prev.loess, newdata = prev.fit2))



akim_inter <- interp(
  x = d_cont$prop_otc,
  y = d_cont$tst_rate,
  # z = d_cont$cml_addi_resist_nia,
  z = d_cont$cml_resist,
  # z = d_cont$cml_pia_all,
  linear = FALSE,
  jitter = TRUE
)

with(
  akim_inter,
  filled.contour(
    x, y, z, nlevels = 200, color.palette = viridis,
    plot.axes = {
      axis(1)
      axis(2)
      contour(x, y, z, add = TRUE, lwd = 1, nlevels = 10, col = "white")
    }
  )
)

d_inter <- tibble(
  x = akim_inter$x,
  y = akim_inter$y
) |>
  expand.grid()
d_inter$z <- as.numeric(t(akim_inter$z))

ggplot(d_inter, aes(x, y)) +
  geom_raster(aes(fill = z), interpolate = TRUE) +
  geom_contour(aes(z = z), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
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
