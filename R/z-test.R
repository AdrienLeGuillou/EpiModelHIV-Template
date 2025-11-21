# Scratchpad for interactive testing before integration in a script

library(dplyr)

d_sc_raw <- readRDS("./data/output/d_raw_edited.rds")
unique(d_raws$scenario_name)

d_raws_edited <- d_raws |>
  filter(scenario_name != "add_otc_same_30") |>
  mutate(
    scenario_name = case_when(
     scenario_name == "add_otc_same0.538813547904572" ~ "add_otc_samex1.3",
     scenario_name == "add_otc_sdur0.107715105354057" ~ "add_otc_sdurx1.10",
     scenario_name == "add_otc_sdur0.220466630000594" ~ "add_otc_sdurx1.20",
     scenario_name == "add_otc_sdur0.342339837375751" ~ "add_otc_sdurx1.30",
     scenario_name == "add_otc_sdur0.469182294946569" ~ "add_otc_sdurx1.40",
     scenario_name == "add_otc_sdur0.596841570180095" ~ "add_otc_sdurx1.50",
     TRUE ~ scenario_name
    )
  )

saveRDS(d_raws_edited, "./data/output/d_raw_edited.rds")