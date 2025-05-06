
################################################################################
# Custom trackers
################################################################################

epi_prep <- function(races_set) {
  function(dat) {
    needed_attributes <- c("race", "prep")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% races_set & prep == 1, na.rm = TRUE)
    })
  }
}

epi_prep_indic <- function(races_set) {
  function(dat) {
    needed_attributes <- c("race", "prep.indic")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% races_set & prep.indic == 1, na.rm = TRUE)
    })
  }
}

epi_prep_otc <- function(races_set) {
  function(dat) {
    needed_attributes <- c("race", "prep.otc")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% races_set & prep.otc == 1, na.rm = TRUE)
    })
  }
}

epi_prep_otc_indic <- function(races_set) {
  function(dat) {
    needed_attributes <- c("race", "prep.otc.indic")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% races_set & prep.otc.indic == 1, na.rm = TRUE)
    })
  }
}

epi_prep_any <- function(races_set) {
  function(dat) {
    needed_attributes <- c("race", "prep.any")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% races_set & prep.any == 1, na.rm = TRUE)
    })
  }
}

epi_prep_both <- function(races_set) {
  function(dat) {
    needed_attributes <- c("race", "prep", "prep.otc")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% races_set & prep == 1 & prep.otc == 1, na.rm = TRUE)
    })
  }
}

epi_gfr_ge60 <- function(races_set) {
  function(dat) {
    needed_attributes <- c("race", "gfr")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% races_set & gfr >= 60, na.rm = TRUE)
    })
  }
}

epi_gfr_otc <- function(races_set) {
  function(dat) {
    needed_attributes <- c("race", "gfr", "prep.otc")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% races_set & gfr < 60 & prep.otc, na.rm = TRUE)
    })
  }
}

epi_gfr_lt60_at <- function(races_set) {
  function(dat) {
    at <- get_current_timestep(dat)
    needed_attributes <- c("race", "gfr.lt60.last")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% races_set & gfr.lt60.last == at, na.rm = TRUE)
    })
  }
}

tk_list <- list(
  gfr_ge60__all = epi_gfr_ge60(1:3),
  gfr_ge60__b = epi_gfr_ge60(1),
  gfr_ge60__h = epi_gfr_ge60(2),
  gfr_ge60__w = epi_gfr_ge60(3),
  gfr_lt60_at__all = epi_gfr_lt60_at(1:3),
  gfr_lt60_at__b = epi_gfr_lt60_at(1),
  gfr_lt60_at__h = epi_gfr_lt60_at(2),
  gfr_lt60_at__w = epi_gfr_lt60_at(3),
  prep__all = epi_prep(1:3),
  prep__b = epi_prep(1),
  prep__h = epi_prep(2),
  prep__w = epi_prep(3),
  prep_indic__all = epi_prep_indic(1:3),
  prep_indic__b = epi_prep_indic(1),
  prep_indic__h = epi_prep_indic(2),
  prep_indic__w = epi_prep_indic(3),
  prep_indic__all = epi_prep_indic(1:3),
  prep_otc__all = epi_prep_otc(1:3),
  prep_otc__b = epi_prep_otc(1),
  prep_otc__h = epi_prep_otc(2),
  prep_otc__w = epi_prep_otc(3),
  prep_otc_indic__all = epi_prep_otc_indic(1:3),
  prep_otc_indic__b = epi_prep_otc_indic(1),
  prep_otc_indic__h = epi_prep_otc_indic(2),
  prep_otc_indic__w = epi_prep_otc_indic(3),
  prep_any__all = epi_prep_any(1:3),
  prep_any__b = epi_prep_any(1),
  prep_any__h = epi_prep_any(2),
  prep_any__w = epi_prep_any(3),
  prep_both__all = epi_prep_both(1:3),
  prep_both__b = epi_prep_both(1),
  prep_both__h = epi_prep_both(2),
  prep_both__w = epi_prep_both(3),

  gfr_otc = epi_gfr_otc(1:3)
)
