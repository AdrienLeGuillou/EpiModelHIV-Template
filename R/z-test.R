# Scratchpad for interactive testing before integration in a script

library(dplyr)

d_raws <- readRDS("./data/output/d_raw.rds")

d <- d_raws |>
  filter(stringr::str_detect(scenario_name, "otc_sdur")) |>
  mutate(or = as.numeric(stringr::str_sub(scenario_name, 10, -1)))
plot(lst_prop_otc ~ or, data = d)


mod <- lm(or ~ poly(lst_prop_otc, 3), data = d)


d_new <- tibble(lst_prop_otc = seq(0.05, 0.55, 0.05))
d_new$or <- predict(mod, newdata = d_new)

c(0.02959476, 0.08424645, 0.13945998, 0.19798574, 0.26257410, 0.33597545, 0.42094017,
0.52021864, 0.63656125, 0.77271838, 0.93144041
)