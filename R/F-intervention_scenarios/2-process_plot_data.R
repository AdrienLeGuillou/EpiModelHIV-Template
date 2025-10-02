library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_light())

source("R/shared_variables.R", local = TRUE)
source("./R/F-intervention_scenarios/z-context.R", local = TRUE)
source("R/F-intervention_scenarios/outcomes.R", local = TRUE)

oopts <- options(future.globals.maxSize = Inf)
if (context == "hpc" && exists("n_cores") && hpc_context) {
  future::plan("multicore", workers = n_cores)
}

scenarios_tibble_dir <- fs::path(scenarios_dir, "merged_tibbles")
d_ref <- make_d_ref(fs::path(scenarios_tibble_dir, "df__baseline.rds"))

b_infos <- EpiModelHPC::get_scenarios_tibble_infos(scenarios_tibble_dir) |>
  filter(
    stringr::str_detect(
      scenario_name,
      "plot_cli[0-9\\.]*_otc[0-9\\.]*_tst[0-9\\.]*"
    )
  ) |>
  mutate(scenario_name_tmp = scenario_name) |>
  separate_wider_regex(
    scenario_name_tmp,
    c("plot_cli[0-9\\.]*_otc[0-9\\.]*_tst", test_p = "[0-9\\.]*")
  )

d_cont <- future.apply::future_lapply(seq_len(nrow(b_infos)), \(i) {
  process_one_scenario(b_infos[i, ], d_ref) |>
    mutate(tst_rate = as.numeric(b_infos[i, "test_p"])) |>
    select(
      tst_rate,
      lst_prop_otc,
      cml_pia_all,
      cml_resist,
      cml_addi_resist_nia
    ) |>
    summarise(across(everything(), median))
}) |>
  bind_rows()

if (!fs::dir_exists(plots_dir)) fs::dir_create(plots_dir)
saveRDS(d_cont, fs::path(plots_dir, "df_cont_plot.R"))
