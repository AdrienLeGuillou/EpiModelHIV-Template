# Scratchpad for interactive testing before integration in a script

library(dplyr)

d_raws <- readRDS("./data/output/d_raw.rds")
unique(d_raws$scenario_name)

# add_no_otc
# add_otc_same
# add_otc_best
# add_otc_sdur
# add_otc_indics
scs_base <- "add_no_otc"
d <- d_raws |>
  filter(stringr::str_detect(scenario_name, scs_base)) |>
  mutate(
    or = as.numeric(stringr::str_replace(scenario_name, scs_base, ""))
  )
plot(lst_prop_otc ~ or, data = d)

add_p <- seq(0.05, 0.50, 0.05)

mod <- lm(or ~ poly(lst_prop_otc, 3), data = d)
d_new <- tibble(lst_prop_otc = add_p)
d_new$or <- predict(mod, newdata = d_new)

# Pedict any PrEP number
plot(lst_prep_any ~ or, data = d)

baseline_any_prep <- 12673
mod <- lm(or ~ poly(lst_prep_any, 3), data = d)
d_new <- tibble(lst_prep_any = baseline_any_prep * (1 + add_p))
d_new$or <- predict(mod, newdata = d_new)
unname(d_new$or)

paste0(
  "ors <- c(\n  ",
  paste0(
    paste0("x", stringr::str_pad(1 + add_p, 4, "right", "0"), " = ", d_new$or),
    collapse = ",\n  "
  ),
  "\n)"
) |>
  clipr::write_clip()