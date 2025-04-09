# Scratchpad for interactive testing before integration in a script
rmarkdown::render(
  "R/Z-calibration/calibration_values.Rmd",
  output_file = "calibration_report.html",
  knit_root_dir = getwd(),
  output_dir = "./"
)
