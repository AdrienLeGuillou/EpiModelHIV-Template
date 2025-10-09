## Intervention Scenarios outcomes
##
## Define helper functions to create the scenarios outcome variables and to
## combine them into digestible tibbles
##
## This script should not be run directly. But `sourced` from other scripts
## within the `R/F-intervention_scenarios/` directory.

# create the elements of the outcomes step by step
mutate_outcomes <- function(d) {
  d |>
    mutate(
      # HIV
      lst_ir100 = ir100,
      cml_incid = incid,
      cml_incid_all = incid,
      lst_ir100_b = ir100.B,
      cml_incid_b = incid.B,
      lst_ir100_h = ir100.H,
      cml_incid_h = incid.H,
      lst_ir100_w = ir100.W,
      cml_incid_w = incid.W,
      lst_hiv_prev_all = i.prev,
      lst_hiv_prev_b = i.prev.B,
      lst_hiv_prev_h = i.prev.H,
      lst_hiv_prev_w = i.prev.W,
      lst_hiv_dx_prop_all = (i_dx__B + i_dx__H + i_dx__W) /
        (i__B + i__H + i__W),
      lst_hiv_dx_prop_b = i_dx__B / i__B,
      lst_hiv_dx_prop_h = i_dx__H / i__H,
      lst_hiv_dx_prop_w = i_dx__W / i__W,

      ## STIs
      lst_ir100_gono = ir100.gono,
      cml_incid_gono = incid.gono,
      lst_ir100_chla = ir100.chla,
      cml_incid_chla = incid.chla,

      ## Clinical PrEP
      lst_prep_num = prepCurr,
      lst_prep_cov = prepCurr / prep.indic,
      lst_prep_elig = prep.indic,
      lst_prep_mean_dur = prep.dur.mean,
      lst_prep_mean_eps = dbg_prep_eps_mean,
      # lst_prep_mean_dur_rng
      # lst_prep_mean_dur_inelig
      # lst_prep_ir100
      # cml_prep_incid

      # OTC PrEP
      lst_prep_otc_num = prep.otcCurr,
      lst_prep_otc_cov = prep.otcCurr / prep.otc.indic,
      lst_prep_otc_elig = prep.otc.indic,
      lst_prep_otc_mean_dur = prep.otc.dur.mean,
      lst_prep_otc_mean_eps = dbg_prep_otc_eps_mean,
      cml_prep_otc_py = prep.otcCurr / 52,
      lst_prep_any = prepCurr + prep.otcCurr,
      lst_prop_otc = prep.otcCurr / lst_prep_any,

      lst_prep_otc_std_indic_cov = prep.otc.std.indic / prep.otcCurr,
      # lst_prep_otc_mean_dur_rng
      # lst_prep_otc_mean_dur_inelig
      # lst_prep_otc_ir100
      # cml_prep_otc_incid
      # GFR --------------------------------------------------------------------
      lst_gfr90_15_30 = `dbg_gfr90_prop_[15,30)`,
      lst_gfr90_30_50 = `dbg_gfr90_prop_[30,50)`,
      lst_gfr90_50_65 = `dbg_gfr90_prop_[50,65)`,
      lst_gfr60_15_30 = `dbg_gfr60_prop_[15,30)`,
      lst_gfr60_30_50 = `dbg_gfr60_prop_[30,50)`,
      lst_gfr60_50_65 = `dbg_gfr60_prop_[50,65)`,
      lst_gfr_drop_ir100 = dbg_gfr_drop / num * 100 * 52,
      cml_gfr_drop = dbg_gfr_drop,
      cml_gfr_drop_gfr90 = dbg_gfr_drop_gfr90,
      cml_gfr_drop_yo50 = dbg_gfr_drop_yo50,

      lst_prep_any_gfr_lt60 = prep_any_gfr_lt60,
      lst_prep_std_gfr_lt60 = prep_std_gfr_lt60,
      lst_prep_otc_gfr_lt60 = prep_otc_gfr_lt60,

      # HBV --------------------------------------------------------------------
      lst_hbv_flare_ir100k = (dbg_hbv_flares_std + dbg_hbv_flares_otc) /
        num *
        1e5 *
        52,
      lst_hbv_flare_otc_ir100k = dbg_hbv_flares_otc / num * 1e5 * 52,
      lst_hbv_flare_std_ir100k = dbg_hbv_flares_std / num * 1e5 * 52,
      cml_hbv_flare = (dbg_hbv_flares_std + dbg_hbv_flares_otc),
      cml_hbv_flare_otc = dbg_hbv_flares_otc,
      cml_hbv_flare_std = dbg_hbv_flares_std,
      # Resistance -------------------------------------------------------------
      cml_resist = any.resist.incid,
      cml_resist_tdf = tdf.resist.incid,
      cml_resist_ftc = ftc.resist.incid,
      cml_resist_prep = any.resist.prep.incid,
      cml_resist_hiv = any.resist.hiv.incid,
      lst_resist_prev = any.resist.prev / num,
      lst_resist_tdf_prev = tdf.resist.prev / num,
      lst_resist_ftc_prev = ftc.resist.prev / num,
      lst_resist_hiv_prev = any.resist.prev / (i__B + i__H + i__W),
      lst_resist_hiv_tdf_prev = tdf.resist.prev / (i__B + i__H + i__W),
      lst_resist_hiv_ftc_prev = ftc.resist.prev / (i__B + i__H + i__W),
      #
      # TODO: separate OTC prep from STD prep?
      #
      # cml_resist_prep
      # cml_resist_prep_tdf
      # cml_resist_prep_ftc
      # cml_resist_hiv
      # cml_resist_hiv_tdf
      # cml_resist_hiv_ftc
      lst_num = num
    )
}

make_d_ref <- function(file_path) {
  readRDS(file_path) |>
    mutate_outcomes() |>
    filter(time >= max(time) - 10 * year_steps) |>
    select(sim, starts_with("cml_incid"), cml_gfr_drop, cml_resist) |>
    group_by(sim) |>
    summarize(across(everything(), \(x) sum(x, na.rm = TRUE))) |>
    ungroup() |>
    select(-sim) |>
    summarize(across(everything(), \(x) median(x, na.rm = TRUE)))
}

mutate_nia_pia <- function(d, ref_val, var, var_nia, var_pia) {
  d[[var_nia]] <- ref_val - d[[var]]
  d[[var_pia]] <- d[[var_nia]] / ref_val
  d
}

mutate_nnt <- function(d, var_nnt, var_nia, var_tt) {
  d[[var_nnt]] <- d[[var_tt]] / d[[var_nia]]
  d
}

# make the outcomes calculated on the same year
make_last_year_outcomes <- function(d) {
  d |>
    filter(time >= max(time) - year_steps) |>
    group_by(scenario_name, sim) |>
    summarise(across(starts_with("lst_"), \(x) mean(x, na.rm = TRUE))) |>
    ungroup()
}

# make the outcomes cumulative over the intervention period
make_cumulative_outcomes <- function(d) {
  d |>
    filter(time >= intervention_start) |>
    group_by(scenario_name, sim) |>
    summarise(across(starts_with("cml_"), \(x) sum(x, na.rm = TRUE))) |>
    ungroup()
}


# each batch of sim is processed in turn
# the output is a data frame with one row per simulation in the batch
# each simulation can be uniquely identified with `scenario_name`,
# `batch_number` and `sim` (all 3 are needed)
process_one_scenario <- function(scenario_infos, d_ref) {
  d_sim <- readRDS(scenario_infos$file_path)
  d_sim <- mutate_outcomes(d_sim)
  d_sim <- mutate(d_sim, scenario_name = scenario_infos$scenario_name)

  d_last <- make_last_year_outcomes(d_sim)
  d_cum <- make_cumulative_outcomes(d_sim)

  d <- left_join(d_last, d_cum, by = c("scenario_name", "sim"))

  for (pop in c("all", "b", "h", "w")) {
    d <- mutate_nia_pia(
      d,
      d_ref[[paste0("cml_incid_", pop)]],
      paste0("cml_incid_", pop),
      paste0("cml_nia_", pop),
      paste0("cml_pia_", pop)
    )
  }

  d <- mutate_nnt(d, "cml_nnt_otc", "cml_nia_all", "cml_prep_otc_py")

  d <- d |>
    mutate(
      cml_hbv_flare_otc_ir100kpy = cml_hbv_flare_otc / cml_prep_otc_py * 1e5,
      cml_addi_resist_nia = (cml_resist - d_ref$cml_resist) / cml_nia_all,
      cml_addi_resist_tdf_nia = (cml_resist_tdf - d_ref$cml_resist_tdf) / cml_nia_all,
      cml_addi_resist_ftc_nia = (cml_resist_ftc - d_ref$cml_resist_ftc) / cml_nia_all,
      cml_addi_hbv_flare_otc_nia = cml_hbv_flare_otc / cml_nia_all,
      cml_addi_gfr_drop_nia = (cml_gfr_drop - d_ref$cml_gfr_drop) / cml_nia_all
    )

  d
}

process_one_scenario_plots <- function(scenario_infos, d_ref) {
  d_sim <- process_one_scenario(scenario_infos, d_ref)
  d_sim |>
    select(scenario_name, starts_with("cml_pia")) |>
    separate_wider_delim(
      scenario_name,
      "_",
      names = c(NA, "test", NA, "treat")
    ) |>
    mutate(test = as.numeric(test), treat = as.numeric(treat))
}
