# =============================================================================
# Export one year of the model for the website (the docs/ folder), the version of
# the app that runs in the browser and can be published on GitHub Pages.
#
# Writes:
#   docs/data/<year>/meta.json  labels, groups, defaults, where each array sits in core.bin
#   docs/data/<year>/core.bin   output, GVA, final demand, GVA by buyer, sales, household loop
#   docs/data/<year>/L.bin      Leontief inverse (3200 x 3200)
#   docs/data/<year>/A.bin      input coefficients, sparse (zeros left out)
#   docs/data/years.json        the years on the website
#   docs/notes.html             the notes page (from app/notes.md)
#
# Numbers are stored as 4-byte floats (about 7 significant digits): half the size of R's
# 8-byte numbers and far more precise than the data. Matrices are stored column by column,
# as R stores them.
#
# Run after prepare_data.R, from the project folder:
#   root_dir <- "."; source("R/03_export_web.R")
# Choose the year with web_year <- 2023 before sourcing (default: the latest year).
# =============================================================================

if (!exists("root_dir")) root_dir <- "."
source(file.path(root_dir, "config.R"))
source(file.path(root_dir, "R/engine.R"))
library(jsonlite)

if (!exists("web_year")) web_year <- max(available_years())
# Input coefficients smaller than this are left out of A.bin. Keep it at 0 (only exact zeros left out):
# many tiny cross-border coefficients add up, e.g. dropping those below 1e-6 moves a third of the
# first-round effect of a German shock on the Baltics to later rounds.
a_threshold <- 0

web_dir <- file.path(root_dir, "docs")
out_dir <- file.path(web_dir, "data", web_year)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

core <- load_core(web_year)
L    <- load_L(web_year)
A    <- load_A(web_year)
hh   <- load_households(web_year)

write_f32 <- function(con, x) writeBin(as.numeric(x), con, size = 4, endian = "little")

# ---- core.bin: several arrays one after another ------------------------------------------
parts <- list(
  x     = core$x,                          # output
  va    = core$va,                         # GVA
  v     = core$v,                          # GVA per EUR of output
  wage  = hh$wage,                         # compensation of employees per EUR of output
  Y     = core$Y,                          # final demand, n x 250 (buyer country x category)
  VAD   = core$VAD,                        # GVA by buyer and category, n x 250
  sales = core$sales_int + core$sales_fd,  # gross sales by buyer country, n x 50
  LH    = hh$LH,                           # output per EUR of household spending, n x 50
  WLH   = hh$WLH,                          # wages per EUR of household spending, 50 x 50
  basic = hh$basic                         # share of household spending at basic prices, 50
)
stopifnot(identical(dim(parts$sales), c(n, n_c)), identical(dim(parts$Y), c(n, n_c * n_k)))
layout <- list(); offset <- 0
con <- file(file.path(out_dir, "core.bin"), "wb")
for (nm in names(parts)) {
  p <- parts[[nm]]
  dims <- if (is.null(dim(p))) length(p) else dim(p)
  layout[[nm]] <- list(offset = offset, length = length(p), dims = I(dims))
  write_f32(con, p)
  offset <- offset + length(p)
}
close(con)

# ---- L.bin -------------------------------------------------------------------------------------
con <- file(file.path(out_dir, "L.bin"), "wb"); write_f32(con, L); close(con)

# ---- A.bin: sparse, column by column ---------------------------------------------------------------
# int32 column starts (n + 1), uint16 row numbers (0-based, padded to 4 bytes), float32 values
keep <- A != 0 & abs(A) >= a_threshold
ptr  <- c(0L, cumsum(colSums(keep)))
pos  <- which(keep)                    # column by column, rows in order
idx  <- (pos - 1L) %% n
vals <- A[pos]
nnz  <- length(pos)
con <- file(file.path(out_dir, "A.bin"), "wb")
writeBin(as.integer(ptr), con, size = 4, endian = "little")
writeBin(as.integer(idx), con, size = 2, endian = "little")
if (nnz %% 2 == 1) writeBin(0L, con, size = 2, endian = "little")
write_f32(con, vals)
close(con)
dropped <- max(colSums(abs(A) * !keep))

# ---- meta.json --------------------------------------------------------------------------------------
broad_groups <- unique(sectors$group)
meta <- list(
  year          = web_year,
  edition       = edition,
  n_countries   = n_c, n_sectors = n_s,
  countries     = data.frame(code = countries$code, name = countries$name),
  sectors       = data.frame(code = sectors$code, name = sectors$name, group = sectors$group),
  fd_categories = data.frame(code = names(fd_categories), label = unname(fd_categories)),
  country_groups = lapply(country_groups, I),
  sector_groups  = lapply(sector_groups, I),
  broad_groups   = I(broad_groups),
  custom_groups  = I(names(custom_sector_groups)),
  defaults = list(origin = default_origin, focus = default_focus, min_va = min_va_eur_m,
                  top_n = top_n, spend_rate = household_spend_rate),
  core = layout,
  A = list(nnz = nnz, threshold = a_threshold),
  files = list(core = "core.bin", L = "L.bin", A = "A.bin"),
  bytes = list(core = file.size(file.path(out_dir, "core.bin")), L = file.size(file.path(out_dir, "L.bin")),
               A = file.size(file.path(out_dir, "A.bin")))
)
write_json(meta, file.path(out_dir, "meta.json"), auto_unbox = TRUE, digits = NA, dataframe = "rows")

# ---- years.json and notes.html ---------------------------------------------------------------------
web_years <- sort(as.integer(list.files(file.path(web_dir, "data"), pattern = "^[0-9]{4}$")))
write_json(list(years = I(web_years), latest = max(web_years)), file.path(web_dir, "data", "years.json"),
           auto_unbox = TRUE)
notes <- commonmark::markdown_html(readLines(file.path(root_dir, "app/notes.md"), encoding = "UTF-8"),
                                   extensions = TRUE)
writeLines(notes, file.path(web_dir, "notes.html"), useBytes = TRUE)

message(sprintf("Exported %d to %s: core %.1f MB, L %.1f MB, A %.1f MB (%.0f%% of coefficients kept; at most %.1e per EUR of output dropped in any column)",
                web_year, out_dir, meta$bytes$core / 1e6, meta$bytes$L / 1e6, meta$bytes$A / 1e6,
                100 * nnz / n^2, dropped))
