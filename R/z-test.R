# Scratchpad for interactive testing before integration in a script

library(dplyr)

d_raws <- readRDS("./data/output/d_raw.rds")

scs_base <- "add_otc_indics"
d <- d_raws |>
  filter(stringr::str_detect(scenario_name, scs_base)) |>
  mutate(
    or = as.numeric(stringr::str_replace( scenario_name, scs_base, ""))
  )
plot(lst_prop_otc ~ or, data = d)

mod <- lm(or ~ poly(lst_prop_otc, 3), data = d)
d_new <- tibble(lst_prop_otc = seq(0.05, 0.55, 0.05))
d_new$or <- predict(mod, newdata = d_new)

# Pedict any PrEP number
plot(lst_prep_any ~ or, data = d)

baseline_any_prep <- 12673
mod <- lm(or ~ poly(lst_prep_any, 3), data = d)
d_new <- tibble(lst_prep_any = baseline_any_prep * seq(1.05, 1.55, 0.05))
d_new$or <- predict(mod, newdata = d_new)
unname(d_new$or)

c(
  0.05267499, 0.10014388, 0.15102485, 0.20502437, 0.26184895,
  0.32120509, 0.38279929, 0.44633804, 0.51152784, 0.57807520
)