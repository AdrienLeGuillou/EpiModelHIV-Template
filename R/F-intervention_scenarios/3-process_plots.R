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
# library(metR)

source("R/shared_variables.R", local = TRUE)
source("./R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/outcomes.R", local = TRUE)

# Process ----------------------------------------------------------------------

d_cont <- readRDS("./data/run/scenarios/plots/df_cont_plot.Rds")

glimpse(d_cont)

library(akima)
akim_inter <- interp(
  x = d_cont$lst_prop_otc,
  y = d_cont$tst_rate,
  z = d_cont$cml_resist,
  linear = F
)

with(akim_inter, contour(x, y, z))

d_inter <- tibble(
  x = akim_inter$x,
  y = akim_inter$y
) |>
  expand.grid()
d_inter$z <- as.numeric(akim_inter$z)

ggplot(d_inter, aes(x, y)) +
  geom_contour(aes(z = z))


data(faithfuld)
glimpse(faithfuld)

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density))
