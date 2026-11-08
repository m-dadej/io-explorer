# =============================================================================
# IO Explorer - settings
# All settings live here. Change a value, re-run prepare_data.R (data) or
# restart the app (everything else).
# =============================================================================

# Folder of the project. Scripts run from the project folder; the app sets this
# to the parent of app/ before it sources this file.
if (!exists("root_dir")) root_dir <- "."

# ---- FIGARO files -------------------------------------------------------------
# Eurostat FIGARO, industry-by-industry, one flat CSV per year, EUR million, current prices.
figaro_dir <- file.path(root_dir, "figaro_tables")
edition    <- "26ed"            # part of the file name: flatfile_eu-ic-io_ind-by-ind_26ed_2024.csv
years      <- 2010:2024

# ---- Folders --------------------------------------------------------------------
labels_dir   <- file.path(root_dir, "data/labels")    # country and industry names, groups
matrices_dir <- file.path(root_dir, "data/matrices")  # the table of each year as matrices (Z, Y, value added)
model_dir    <- file.path(root_dir, "data/model")     # Leontief inverse and results used by the app
tables_dir   <- file.path(root_dir, "data/tables")    # long tables (parquet) for your own analysis

# ---- Final demand categories (FIGARO codes) ----------------------------------------
# The order here is the order in charts and tables.
fd_categories <- c(
  P3_S14 = "Households",
  P3_S13 = "Government",
  P3_S15 = "NPISH",
  P51G   = "Investment (GFCF)",
  P5M    = "Inventories & valuables"
)

# ---- Country groups ---------------------------------------------------------------
# Use the group name anywhere you can pick a country (app and engine functions).
eu27 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU",
          "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

country_groups <- list(
  "CEE11"           = c("PL", "CZ", "SK", "HU", "RO", "BG", "HR", "SI", "EE", "LV", "LT"),
  "Visegrad 4"      = c("PL", "CZ", "SK", "HU"),
  "Baltics"         = c("EE", "LV", "LT"),
  "Western Balkans" = c("AL", "ME", "MK", "RS"),
  "EU27"            = eu27,
  "Euro area"       = c("AT", "BE", "BG", "CY", "DE", "EE", "ES", "FI", "FR", "GR", "HR", "IE",
                        "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK"),  # 2026 membership
  "EU27 excl. CEE11" = setdiff(eu27, c("PL", "CZ", "SK", "HU", "RO", "BG", "HR", "SI", "EE", "LV", "LT")),
  "G7"              = c("US", "JP", "DE", "FR", "GB", "IT", "CA"),
  "Non-EU"          = c("AL", "AR", "AU", "BR", "CA", "CH", "CN", "GB", "ID", "IN", "JP", "KR", "ME",
                        "MK", "MX", "NO", "RS", "RU", "SA", "TR", "US", "ZA", "FIGW1"),
  "World"           = NULL  # filled in by the engine: all 50 areas
)

# ---- Industry groups ------------------------------------------------------------------
# Broad groups come from data/labels/sectors.csv (column `group`). Add your own here:
# any name, any list of FIGARO industry codes.
custom_sector_groups <- list(
  "Market services"                = c("G45", "G46", "G47", "H49", "H50", "H51", "H52", "H53", "I",
                                       "J58", "J59_60", "J61", "J62_63", "K64", "K65", "K66", "L",
                                       "M69_70", "M71", "M72", "M73", "M74_75",
                                       "N77", "N78", "N79", "N80T82"),
  "Energy-intensive manufacturing" = c("C17", "C19", "C20", "C23", "C24"),
  "Automotive supply chain"        = c("C22", "C24", "C25", "C27", "C28", "C29"),
  "Metals & machinery"             = c("C24", "C25", "C27", "C28"),
  "Defence-related manufacturing"  = c("C25", "C26", "C30")  # weapons & ammo (25.4), electronics, military vehicles (30.4)
)

# ---- Household spending of extra wages (optional in shocks) -----------------------------------
# When this option is on, households spend part of the extra wages a shock creates. That spending
# creates more output and more wages, and so on (an "induced" or Type II effect). The value below
# is how many EUR of consumer spending (VAT included) each extra EUR of compensation of employees
# brings. It combines two things:
#   - what reaches households after employer and employee contributions and income tax
#     (for Poland roughly 0.55-0.6 of compensation)
#   - the share of that net income that is spent (roughly 0.7-0.9)
# One value for all countries. In the engine you can also give a named vector, e.g.
# c(PL = 0.45, HU = 0.40); countries not named keep this value.
household_spend_rate <- 0.45

# ---- Rankings -----------------------------------------------------------------------------
# Country-sectors with less value added than this (EUR million) are left out of rankings
# of shares; tiny sectors otherwise top the lists with extreme shares.
min_va_eur_m <- 100
top_n        <- 20   # rows shown in the "top" charts and tables

# ---- Defaults in the app -------------------------------------------------------------------
default_origin  <- "PL"
default_focus   <- "CEE11"   # countries shown in rankings by default
default_year    <- max(years)
