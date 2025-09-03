# Setup -----------------------------------------------------------------------
library(dplyr)
library(tidyr)

source("R/shared_variables.R", local = TRUE)
source("R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/labels.R", local = TRUE)

d_raw <- readr::read_csv("data/output/table.csv")
d_clean_scs <- d_raw[order_scs(d_raw$scenario_name), ] |>
  mutate(scenario_name = nicefy_scs_names(scenario_name))
readr::write_csv(d_clean_scs, "data/output/clean_scs_table.csv")

# First Table ------------------------------------------------------------------
first_table_labels <- c(
  # Process
  "lst_prep_any",
  "lst_prep_otc_num",
  # Epi
  # "lst_ir100",
  # "cml_incid",
  # "cml_nia_all",
  "cml_pia_all",
  # "cml_nnt_otc",
  "lst_resist_hiv_prev",
  # "cml_gfr_drop",
  # "cml_gfr_drop_gfr90",
  # "cml_gfr_drop_yo50",
  "lst_prep_any_gfr_lt60",
  # "cml_hbv_flare",
  "cml_hbv_flare",
  "cml_hbv_flare_otc"
)

labels <- var_labels[first_table_labels]

first_table_scs <- tibble(
  scenario_name = c(
    "baseline",
    "base_only_otc_same_adhr_base",
    "only_otc_relaxed_adhr_base",
    "only_otc_best_adhr_base",
    "no_otc_prep_or150",
    "otc_best_adhr_base",
    "otc_mix_adhr_base"
  )
)

d_first <- first_table_scs |>
  left_join(d_raw, by = "scenario_name") |>
  select(scenario_name, any_of(unname(labels))) |>
  mutate(scenario_name = nicefy_scs_names(scenario_name))

readr::write_csv(d_first, "data/output/table1.csv")

d_full_basic <- first_table_scs |>
  left_join(d_raw, by = "scenario_name") |>
  mutate(scenario_name = nicefy_scs_names(scenario_name))
readr::write_csv(d_full_basic, "data/output/table_basics.csv")


# Per Sub Tables ---------------------------------------------------------------

sub_table_names <- c(
  "base_only_otc_same",
  "only_otc_relaxed",
  "only_otc_best",
  "otc_mix",
  "otc_best"
)
for (tbl in sub_table_names) {
  d_raw_t <- readr::read_csv(paste0("data/output/table__", tbl, ".csv"))

  d_t <- d_raw_t[order_scs(d_raw_t$scenario_name), ] |>
    select(scenario_name, any_of(unname(labels))) |>
    mutate(scenario_name = nicefy_scs_names(scenario_name))

  readr::write_csv(d_t, paste0("data/output/table1__", tbl, ".csv"))
}
