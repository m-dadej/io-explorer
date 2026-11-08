# =============================================================================
# 02 Leontief model
# For each year:
#   A = Z / x            input coefficients (what each country-sector buys per EUR of output)
#   L = (I - A)^-1       Leontief inverse: output everywhere needed per EUR of final demand
#   v = GVA / x          value added per EUR of output
#   VAD = v * (L Y)      value added of each country-sector (rows) absorbed by the final demand
#                        of each buyer country and category (columns): "trade in value added"
# Saves data/model/L_<year>.rds (the inverse, about 80 MB) and data/model/core_<year>.rds
# (everything else the app needs), and long tables for your own analysis in data/tables/.
# Takes about 1 minute per year.
# =============================================================================

source(file.path(root_dir, "config.R"))
library(data.table)

dir.create(model_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

countries <- fread(file.path(labels_dir, "countries.csv"), encoding = "UTF-8")
sectors   <- fread(file.path(labels_dir, "sectors.csv"),   encoding = "UTF-8")
n_c <- nrow(countries); n_s <- nrow(sectors); n <- n_c * n_s; n_k <- length(fd_categories)

gva_rows <- c("D1", "D29X39", "B2A3G")   # compensation + other taxes on production + operating surplus

# Indicator matrices that sum columns to buyer countries
by_country_cs <- diag(n_c)[rep(seq_len(n_c), each = n_s), ]   # 3200 x 50
by_country_fd <- diag(n_c)[rep(seq_len(n_c), each = n_k), ]   # 250 x 50

for (year in years) {
  in_file   <- file.path(matrices_dir, sprintf("figaro_%d.rds", year))
  L_file    <- file.path(model_dir, sprintf("L_%d.rds", year))
  core_file <- file.path(model_dir, sprintf("core_%d.rds", year))
  if (!file.exists(in_file)) next
  if (file.exists(core_file) && file.mtime(core_file) > file.mtime(in_file)) next

  m <- readRDS(in_file)
  x <- m$x
  inv_x <- ifelse(x > 0, 1 / x, 0)    # 67 country-sectors (e.g. "households as employers" in some countries) have no output

  A <- m$Z * rep(inv_x, each = n)     # divide each column by the buyer's output
  L <- solve(diag(n) - A)
  dimnames(L) <- dimnames(m$Z)

  va <- colSums(m$W[gva_rows, ])
  v  <- va * inv_x
  VAD <- v * (L %*% m$Y)              # 3200 x 250

  sales_int <- m$Z %*% by_country_cs  # sales to industries of each buyer country, 3200 x 50
  sales_fd  <- m$Y %*% by_country_fd  # sales to final users of each buyer country, 3200 x 50
  colnames(sales_int) <- colnames(sales_fd) <- countries$code

  # Checks (largest gap, EUR million): L reproduces output from final demand, and every
  # country-sector's value added is fully allocated to some buyer (sum over buyers = its GVA).
  # A few tiny country-sectors with negative output (e.g. AL_T in 2017, -EUR 2,000) are treated as
  # zero output, which leaves gaps of a few hundred euros.
  gap_x  <- max(abs(L %*% rowSums(m$Y) - x))
  gap_va <- max(abs(rowSums(VAD) - va)[x > 0])
  if (gap_x > 0.01 || gap_va > 0.01) {
    warning(year, ": model checks failed (output gap EUR ", signif(gap_x, 2), " m, VA gap EUR ", signif(gap_va, 2), " m)")
  }

  saveRDS(L, L_file, compress = FALSE)
  saveRDS(list(year = year, x = x, va = va, v = v, va_comp = m$W, Y = m$Y, VAD = VAD,
               sales_int = sales_int, sales_fd = sales_fd,
               min_L_diag = min(diag(L))),
          core_file)
  message(year, ": Leontief model done (largest gaps: output EUR ", signif(gap_x, 2), " m, VA EUR ", signif(gap_va, 2), " m)")
  rm(m, A, L, VAD); gc(verbose = FALSE)
}

# ---- Long tables for your own analysis (parquet; open with arrow::read_parquet) --------
cs_country <- factor(rep(countries$code, each = n_s), levels = countries$code)
cs_sector  <- factor(rep(sectors$code, times = n_c),  levels = sectors$code)
fd_dest    <- factor(rep(countries$code, each = n_k), levels = countries$code)
fd_cat     <- factor(rep(unname(fd_categories), times = n_c), levels = unname(fd_categories))

va_dest <- list(); sales <- list(); totals <- list()
for (year in years) {
  core_file <- file.path(model_dir, sprintf("core_%d.rds", year))
  if (!file.exists(core_file)) next
  core <- readRDS(core_file)

  # Value added by origin country-sector x buyer country x final demand category
  va_dest[[as.character(year)]] <- data.table(
    year     = year,
    country  = rep(cs_country, times = n_c * n_k),
    sector   = rep(cs_sector,  times = n_c * n_k),
    buyer    = rep(fd_dest, each = n),
    category = rep(fd_cat,  each = n),
    va       = as.vector(core$VAD)
  )[va != 0]

  # Direct (gross) sales by origin country-sector x buyer country
  sales[[as.character(year)]] <- data.table(
    year         = year,
    country      = rep(cs_country, times = n_c),
    sector       = rep(cs_sector,  times = n_c),
    buyer        = rep(factor(countries$code, levels = countries$code), each = n),
    intermediate = as.vector(core$sales_int),
    final        = as.vector(core$sales_fd)
  )[intermediate != 0 | final != 0]

  totals[[as.character(year)]] <- data.table(
    year = year, country = cs_country, sector = cs_sector,
    output = core$x, gva = core$va,
    compensation       = core$va_comp["D1", ],
    other_net_taxes    = core$va_comp["D29X39", ],
    operating_surplus  = core$va_comp["B2A3G", ],
    product_taxes_paid = core$va_comp["D21X31", ]
  )
}

arrow::write_parquet(rbindlist(va_dest), file.path(tables_dir, "va_by_buyer.parquet"))
arrow::write_parquet(rbindlist(sales),   file.path(tables_dir, "direct_sales.parquet"))
arrow::write_parquet(rbindlist(totals),  file.path(tables_dir, "totals.parquet"))
message("Long tables written to ", tables_dir)
