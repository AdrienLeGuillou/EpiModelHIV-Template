## Tables Labels and Format
##
## Define how the name of the variable should be rendered in the tables. Define
## what number format should be used for each variable
##
## This script should not be run directly. But `sourced` from other scripts
## within the `R/F-intervention_scenarios/` directory.

# Conversion between variable name and final label
var_labels <- c(
  # Epi
  "lst_ir100"    = "HIV IR100 All (ly)",
  "lst_ir100_b"  = "HIV IR100 Black (ly)",
  "lst_ir100_h"  = "HIV IR100 Hispanic (ly)",
  "lst_ir100_w"  = "HIV IR100 White (ly)",

  "cml_incid"    = "HIV Cumulative Incidence All (10y)",
  "cml_incid_b"  = "HIV Cumulative Incidence Black (10y)",
  "cml_incid_h"  = "HIV Cumulative Incidence Hispanic (10y)",
  "cml_incid_w"  = "HIV Cumulative Incidence White (10y)",

  "lst_ir100_gono"    = "Gono IR100 All (ly)",
  "lst_ir100_chla"    = "Chla IR100 All (ly)",

  "cml_incid_gono"    = "Gono Cumulative Incidence All (10y)",
  "cml_incid_chla"    = "Chla Cumulative Incidence All (10y)",

  "lst_prep_any"      = "Any PrEP Number (ly)",

  "lst_prep_cov"      = "Clinical PrEP Coverage (ly)",
  "lst_prep_elig"     = "Clinical PrEP Eligibles (ly)",
  "lst_prep_mean_dur" = "Clinical PrEP Mean Duration (ly)",
  "lst_prep_mean_eps" = "Clinical PrEP Mean Number of Unique Episodes (ly)",

  "lst_prep_otc_cov"      = "OTC PrEP Coverage (ly)",
  "lst_prep_otc_elig"     = "OTC PrEP Eligibles (ly)",
  "lst_prep_otc_mean_dur" = "OTC PrEP Mean Duration (ly)",
  "lst_prep_otc_mean_eps" = "OTC PrEP Mean Number of Unique Episodes (ly)",

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
  "lst_prep_std" = "Proportion of PrEP user (std) with GFR < 60 (ly)",
  "lst_prep_otc_gfr_lt60" = "Proportion of PrEP user (otc) with GFR < 60 (ly)",

  "lst_hbv_flare_ir100"     = "HBV Flares IR100 (ly)",
  "lst_hbv_flare_otc_ir100" = "HBV Flares IR100 - OTC PrEP(ly)",
  "lst_hbv_flare_std_ir100" = "HBV Flares IR100 - STD PrEP(ly)",
  "cml_hbv_flare"           = "HBV Flares Cumulative (10y)",
  "cml_hbv_flare_otc"       = "HBV Flares Cumulative - OTC PrEP(10y)",
  "cml_hbv_flare_std"       = "HBV Flares Cumulative - STD PrEP(10y)",

  "cml_resist" = "Any ART resistance Cumulative (10y)",
  "cml_resist_tdf" = "TDF resistance Cumulative (10y)",
  "cml_resist_ftc" = "FTC resistance Cumulative (10y)",
  "cml_resist_prep" = "Any ART resistance Cumulative - HIV inf while PrEP (10y)",
  "cml_resist_hiv" = "Any ART resistance Cumulative - PrEP start while HIV (10y)",
  "lst_resist_prev" = "Any ART resistance Prevalence (ly)",
  "lst_resist_tdf_prev" = "TDF resistance Prevalence (ly)",
  "lst_resist_ftc_prev" = "FTC resistance Prevalence (ly)",

  "lst_num" = "num"
)

unused_labels <- c(
  "cml_nia"      = "HIV NIA All (10y)",
  "cml_nia_b"    = "HIV NIA Black (10y)",
  "cml_nia_h"    = "HIV NIA Hispanic (10y)",
  "cml_nia_w"    = "HIV NIA White (10y)",

  "cml_pia"      = "HIV PIA All (10y)",
  "cml_pia_b"    = "HIV PIA Black (10y)",
  "cml_pia_h"    = "HIV PIA Hispanic (10y)",
  "cml_pia_w"    = "HIV PIA White (10y)",

  "cml_nnt_b"    = "HIV NNT Black (10y)",
  "cml_nnt_h"    = "HIV NNT Hispanic (10y)",
  "cml_nnt_w"    = "HIV NNT White (10y)"
)

# Formatters for the variables
fmts <- replicate(length(var_labels), scales::label_number(1))
names(fmts) <- names(var_labels)

format_patterns <- list(
  small_num = list(
    patterns = c("lst_ir100", "lst_.*_ir100", "cml_nnt", ".*_mean_eps"),
    fun = scales::label_number(0.01)
  ),
  small_perc = list(
    patterns = c("lst_resist.*_prev"),
    fun = scales::label_percent(0.01)
  ),
  perc = list(
    patterns = c("cml_pia", "lst_.*_prev", "lst_.*_cov", "lst_gfr[69].*"),
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

make_ordered_labels <- function(nms, named_labels) {
  ordered_labels <- named_labels[nms]
  ordered_labels <- paste0(seq_along(ordered_labels), "-", ordered_labels)
  names(ordered_labels) <- nms

  ordered_labels
}

### utils-format.R
library(dplyr)
library(tidyr)

format_table <- function(d, var_labels, format_patterns) {
  formatters <- make_formatters(var_labels, format_patterns)

  d_out <- d |>
    sum_quants(0.025, 0.5, 0.975) |>
    pivot_longer(-scenario_name) |>
    separate(name, into = c("name", "quantile"), sep = "_/_") |>
    pivot_wider(names_from = quantile, values_from = value) |>
    filter(name %in% names(var_labels)) |>
    mutate(
      clean_val = purrr::pmap_chr(
        list(name, l, m, h),
        ~ common_format(formatters, ..1, ..2, ..3, ..4))
    ) |>
    select(-c(l, m, h)) |>
    mutate(
      name = var_labels[name]
    ) |>
    pivot_wider(names_from = name, values_from = clean_val) |>
    arrange(scenario_name)

  reorder_cols(d_out, var_labels)
}

make_formatters <- function(var_labels, format_patterns) {
  fmts <- vector(mode = "list", length = length(var_labels))
  for (nms in names(var_labels)) {
    for (fp in format_patterns) {
      if (any(stringr::str_detect(nms, fp$patterns))) {
        fmts[[nms]] <- fp$fun
        break()
      }
    }
  }
  fmts
}


sum_quants <- function(d, ql = 0.025, qm = 0.5, qh = 0.975) {
  d |>
    ungroup() |>
    select(-sim) |>
    group_by(scenario_name) |>
    summarise(across(
      everything(),
      list(
        l = ~ quantile(.x, ql, na.rm = TRUE),
        m = ~ quantile(.x, qm, na.rm = TRUE),
        h = ~ quantile(.x, qh, na.rm = TRUE)
      ),
      .names = "{.col}_/_{.fn}"
    ),
    .groups = "drop"
  )
}


reorder_cols <- function(d, var_labels) {
  missing_cols <- setdiff(names(d), var_labels)
  cols_order <- c(missing_cols, intersect(var_labels, names(d)))
  d[, cols_order]
}

common_format <- function(formatters, name, ql, qm, qh) {
  if (is.na(qm)) {
    "-"
  } else {
    paste0(
        formatters[[name]](qm), " (", formatters[[name]](ql),
        ", ", formatters[[name]](qh), ")"
    )
  }
}
