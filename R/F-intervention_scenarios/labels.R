## Tables Labels and Format
##
## Define how the name of the variable should be rendered in the tables. Define
## what number format should be used for each variable
##
## This script should not be run directly. But `sourced` from other scripts
## within the `R/F-intervention_scenarios/` directory.

source("R/F-intervention_scenarios/utils-formats.R", local = TRUE)

# Conversion between variable name and final label
var_labels <- c(
  # Epi
  "lst_ir100" = "HIV IR100 All (ly)",
  "lst_ir100_b" = "HIV IR100 Black (ly)",
  "lst_ir100_h" = "HIV IR100 Hispanic (ly)",
  "lst_ir100_w" = "HIV IR100 White (ly)",

  "cml_nia_all" = "HIV NIA All (10y)",
  "cml_nia_b" = "HIV NIA Black (10y)",
  "cml_nia_h" = "HIV NIA Hispanic (10y)",
  "cml_nia_w" = "HIV NIA White (10y)",

  "cml_pia_all" = "HIV PIA All (10y)",
  "cml_pia_b" = "HIV PIA Black (10y)",
  "cml_pia_h" = "HIV PIA Hispanic (10y)",
  "cml_pia_w" = "HIV PIA White (10y)",

  "cml_nnt_otc" = "NNT HIV PY on OTC (10y)",

  "cml_incid" = "HIV Cumulative Incidence All (10y)",
  "cml_incid_b" = "HIV Cumulative Incidence Black (10y)",
  "cml_incid_h" = "HIV Cumulative Incidence Hispanic (10y)",
  "cml_incid_w" = "HIV Cumulative Incidence White (10y)",

  "lst_hiv_prev_all" = "HIV Prevalence All (ly)",
  "lst_hiv_prev_b" = "HIV Prevalence Black (ly)",
  "lst_hiv_prev_h" = "HIV Prevalence Hispanic (ly)",
  "lst_hiv_prev_w" = "HIV Prevalence White (ly)",

  "lst_hiv_dx_prop_all" = "Prop HIV Diag All (ly)",
  "lst_hiv_dx_prop_b" = "Prop HIV Diag Black (ly)",
  "lst_hiv_dx_prop_h" = "Prop HIV Diag Hispanic (ly)",
  "lst_hiv_dx_prop_w" = "Prop HIV Diag White (ly)",

  "lst_ir100_gono" = "Gono IR100 All (ly)",
  "lst_ir100_chla" = "Chla IR100 All (ly)",

  "cml_incid_gono" = "Gono Cumulative Incidence All (10y)",
  "cml_incid_chla" = "Chla Cumulative Incidence All (10y)",

  "lst_prep_any" = "Any PrEP Number (ly)",
  "lst_prop_otc" = "Proportion of OTC (ly)",
  "lst_prep_num" = "STD PrEP Number (ly)",
  "lst_prep_otc_num" = "OTC PrEP Number (ly)",

  "lst_prep_cov" = "Clinical PrEP Coverage (ly)",
  "lst_prep_elig" = "Clinical PrEP Eligibles (ly)",
  "lst_prep_mean_dur" = "Clinical PrEP Mean Duration (ly)",
  "lst_prep_mean_eps" = "Clinical PrEP Mean Number of Unique Episodes (ly)",

  "lst_prep_otc_cov" = "OTC PrEP Coverage (ly)",
  "lst_prep_otc_elig" = "OTC PrEP Eligibles (ly)",
  "lst_prep_otc_mean_dur" = "OTC PrEP Mean Duration (ly)",
  "lst_prep_otc_mean_eps" = "OTC PrEP Mean Number of Unique Episodes (ly)",
  "lst_prep_otc_std_indic_cov" = "OTC PrEP users indicated to Clinical PrEP (ly)",

  "lst_gfr90_15_30" = "Proportion of [15, 30) with GFR < 90 (ly)",
  "lst_gfr90_30_50" = "Proportion of [30, 50) with GFR < 90 (ly)",
  "lst_gfr90_50_65" = "Proportion of [50, 65) with GFR < 90 (ly)",
  "lst_gfr60_15_30" = "Proportion of [15, 30) with GFR < 60 (ly)",
  "lst_gfr60_30_50" = "Proportion of [30, 50) with GFR < 60 (ly)",
  "lst_gfr60_50_65" = "Proportion of [50, 65) with GFR < 60 (ly)",

  "lst_gfr_drop_ir100" = "GFR drop due to PrEP IR100 (ly)",
  "cml_gfr_drop" = "Cumulative GFR drop due to PrEP (10y)",
  "cml_gfr_drop_gfr90" = "Cumulative GFR drop prev GFR < 90  (10y)",
  "cml_gfr_drop_yo50" = "Cumulative GFR drop age > 50yo (10y)",

  "lst_prep_any_gfr_lt60" = "Proportion of PrEP user (any) with GFR < 60 (ly)",
  "lst_prep_std_gfr_lt60" = "Proportion of PrEP user (std) with GFR < 60 (ly)",
  "lst_prep_otc_gfr_lt60" = "Proportion of PrEP user (otc) with GFR < 60 (ly)",

  "lst_hbv_flare_ir100k" = "HBV Flares IR100k (ly)",
  "lst_hbv_flare_std_ir100k" = "HBV Flares IR100k - STD PrEP(ly)",
  "lst_hbv_flare_otc_ir100k" = "HBV Flares IR100k - OTC PrEP(ly)",
  "cml_hbv_flare" = "HBV Flares Cumulative (10y)",
  "cml_hbv_flare_otc" = "HBV Flares Cumulative - OTC PrEP (10y)",
  "cml_hbv_flare_std" = "HBV Flares Cumulative - STD PrEP (10y)",
  "cml_hbv_flare_otc_ir100kpy" = "HBV Flares OTC IR100k PY (10y)",

  "cml_resist" = "Any ART resistance Cumulative (10y)",
  "cml_resist_tdf" = "TDF resistance Cumulative (10y)",
  "cml_resist_ftc" = "FTC resistance Cumulative (10y)",
  "cml_resist_prep" = "Any ART resistance Cumulative - HIV inf while PrEP (10y)",
  "cml_resist_hiv" = "Any ART resistance Cumulative - PrEP start while HIV (10y)",
  "lst_resist_prev" = "Any ART resistance Prevalence (all pop) (ly)",
  "lst_resist_tdf_prev" = "TDF resistance Prevalence  (all pop) (ly)",
  "lst_resist_ftc_prev" = "FTC resistance Prevalence (all pop) (ly)",
  "lst_resist_hiv_prev" = "Any ART resistance Prevalence (among HIV+) (ly)",
  "lst_resist_hiv_tdf_prev" = "TDF resistance Prevalence (among HIV+) (ly)",
  "lst_resist_hiv_ftc_prev" = "FTC resistance Prevalence (among HIV+) (ly)",

  "cml_addi_resist_nia" = "Additional Resistances Created per Infection Averted (10y)",
  "cml_addi_hbv_flare_otc_nia" = "Additional of HBV Flares Due to OTC per Infection Averted (10y)",
  "cml_addi_gfr_drop_nia" = "Additional GFR Drops Due to PrEP per Infection Averted (10y)",

  "lst_num" = "num"
)

unused_labels <- c(
  "cml_nia" = "HIV NIA All (10y)",
  "cml_nia_b" = "HIV NIA Black (10y)",
  "cml_nia_h" = "HIV NIA Hispanic (10y)",
  "cml_nia_w" = "HIV NIA White (10y)",

  "cml_pia" = "HIV PIA All (10y)",
  "cml_pia_b" = "HIV PIA Black (10y)",
  "cml_pia_h" = "HIV PIA Hispanic (10y)",
  "cml_pia_w" = "HIV PIA White (10y)",

  "cml_nnt_b" = "HIV NNT Black (10y)",
  "cml_nnt_h" = "HIV NNT Hispanic (10y)",
  "cml_nnt_w" = "HIV NNT White (10y)"
)


# Formatters for the variables
fmts <- replicate(length(var_labels), scales::label_number(1))
names(fmts) <- names(var_labels)

format_patterns <- list(
  small_num = list(
    patterns = c(
      "lst_ir100",
      "lst_.*_ir100",
      "cml_addi_",
      "cml_nnt",
      ".*_mean_eps"
    ),
    fun = scales::label_number(0.01)
  ),
  small_perc = list(
    patterns = c("lst_resist.*_prev"),
    fun = scales::label_percent(0.01)
  ),
  perc = list(
    patterns = c(
      "cml_pia",
      "lst_.*_prev",
      "lst_.*_cov",
      "lst_gfr[69].*",
      "lst_prep_.*_gfr_lt60",
      "lst.*_prop"
    ),
    fun = scales::label_percent(0.1)
  ),
  default = list(
    patterns = ".*",
    fun = scales::label_number(1)
  )
)

for (nms in names(fmts)) {
  for (fp in format_patterns) {
    if (any(stringr::str_detect(nms, fp$patterns))) {
      fmts[[nms]] <- fp$fun
      break()
    }
  }
}

scenarios_root_names <- c(
  "baseline" = "Standard PrEP Only",
  "no_otc_prep" = "Standard PrEP Only",
  "base_only_otc_same" = "Substituting STD with OTC - Same",
  "only_otc_same" = "Substituting STD with OTC - Same",
  "only_otc_relaxed" = "Substituting STD with OTC - Relaxed Indications",
  "only_otc_best" = "Substituting STD with OTC - Best Guess",
  "otc_mix" = "Adding OTC PrEP Best Guess - Lower STD Start Rate",
  "otc_best" = "Adding OTC PrEP Best Guess - Keep STD Start Rate"
)

scenarios_prefix_names <- c(
  "_or125" = ": OR 1.25",
  "_or150" = ": OR 1.5",
  "_or155" = ": OR 1.55",
  "_or175" = ": OR 1.75",
  "_or200" = ": OR 2",
  "_adhr_m20" = ": High Adherence -20%",
  "_adhr_m10" = ": High Adherence -10%",
  "_adhr_m05" = ": High Adherence -5%",
  "_adhr_base" = "",
  "_adhr_p05" = ": High Adherence +5%",
  "_adhr_p10" = ": High Adherence +10%",
  "_adhr_p20" = ": High Adherence +20%",
  "_disc_175" = ": Time to Discontinuation x1.75",
  "_disc_150" = ": Time to Discontinuation x1.5",
  "_disc_125" = ": Time to Discontinuation x1.25",
  "_disc_100" = ": Time to Discontinuation x1",
  "_disc_075" = ": Time to Discontinuation x0.75",
  "_disc_050" = ": Time to Discontinuation x0.5",
  "_disc_025" = ": Time to Discontinuation x0.25",
  "_disc01" = ": Time to Discontinuation x0.1",
  "_disc02" = ": Time to Discontinuation x0.2",
  "_disc03" = ": Time to Discontinuation x0.3",
  "_disc04" = ": Time to Discontinuation x0.4",
  "_gfr_same" = ": GFR Test as Recommended",
  "_gfr_1" = ": GFR Test Every 1 year",
  "_gfr_2" = ": GFR Test Every 2 year",
  "_gfr_3" = ": GFR Test Every 3 year",
  "_gfr_5" = ": GFR Test Every 5 year",
  "_gfr_Inf" = ": GFR Test Never",
  "_hivtst_13" = ": HIV Test Every 3 Months",
  "_hivtst_26" = ": HIV Test Every 6 Months",
  "_hivtst_52" = ": HIV Test Every 12 Months",
  "_stitst_13" = ": STI Test Every 3 Months",
  "_stitst_26" = ": STI Test Every 6 Months",
  "_stitst_52" = ": STI Test Every 12 Months",
  "_some_hivtst_25" = ": Some (25%) HIV Test at OTC Start",
  "_some_hivtst_50" = ": Some (50%) HIV Test at OTC Start",
  "_some_hivtst_75" = ": Some (75%) HIV Test at OTC Start",
  "_always_hivtst" = ": Always Test HIV at OTC Start",
  "_always_stitst" = ": Always Test STI at OTC Start",
  "_always_bothtst" = ": Always Test HIV and STI at OTC Start",
  "_100" = ": OR 1",
  "_125" = ": OR 1.25",
  "_150" = ": OR 1.5",
  "_175" = ": OR 1.75",
  "_200" = ": OR 2"
)

nicefy_scs_names <- function(scs_names) {
  stringr::str_replace_all(scs_names, scenarios_root_names) |>
    stringr::str_replace_all(scenarios_prefix_names)
}

order_scs <- function(scs_names) {
  root_order <- stringr::str_pad(
    seq_along(scenarios_root_names),
    2,
    "left",
    "0"
  )
  names(root_order) <- names(scenarios_root_names)
  prefix_order <- stringr::str_pad(
    seq_along(scenarios_prefix_names),
    2,
    "left",
    "0"
  )
  names(prefix_order) <- names(scenarios_prefix_names)
  sc_order <- stringr::str_replace_all(scs_names, root_order) |>
    stringr::str_replace_all(prefix_order)
  order(sc_order)
}
