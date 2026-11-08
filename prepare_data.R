# =============================================================================
# IO Explorer - prepare the data (run once, and again when a new FIGARO edition arrives)
#
# Open io-explorer.Rproj in RStudio and run this file (Ctrl+Shift+S).
# Needs the FIGARO flat files in figaro_tables/ (see config.R for the edition and years).
# Takes about 15 minutes for 15 years and about 3 GB of disk.
# Years already done are skipped; delete data/matrices or data/model to redo them.
# =============================================================================

root_dir <- "."
source("config.R")

source("R/01_import.R")     # CSV -> matrices (Z, Y, value added) per year
source("R/02_leontief.R")   # Leontief inverse, value added by buyer, long tables

message("Data ready. Start the app with: shiny::runApp('app')")
