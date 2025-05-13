
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

make_strat_trackers <- function(base_name, tracker) {
  rset <- c(1:3, 1, 2, 3)
  out <- vector(mode = "list", length = 4)
  names(out) <- paste0(base_name, "__", c("all", "b", "h", "w"))
  for (i in seq_len(4)) {
    out[[i]] <- tracker(rset[i])
  }
  out
}

tk_list <- list()

tk_list <- c(tk_list, make_strat_trackers("gfr_ge60", epi_gfr_ge60))
tk_list <- c(tk_list, make_strat_trackers("gfr_lt60_at", epi_gfr_lt60_at))
tk_list <- c(tk_list, make_strat_trackers("gfr_otc", epi_gfr_otc))

tk_list <- c(tk_list, make_strat_trackers("prep", epi_prep))
tk_list <- c(tk_list, make_strat_trackers("prep_indic", epi_prep_indic))
tk_list <- c(tk_list, make_strat_trackers("prep_otc", epi_prep_otc))
tk_list <- c(tk_list, make_strat_trackers("prep_otc_indic", epi_prep_otc_indic))
tk_list <- c(tk_list, make_strat_trackers("prep_any", epi_prep_any))
tk_list <- c(tk_list, make_strat_trackers("prep_both", epi_prep_both))
tk_list <- c(tk_list, make_strat_trackers("prep_both", epi_prep_both))
