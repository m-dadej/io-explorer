# =============================================================================
# 01 Import
# Reads each FIGARO flat file (one per year, about 11 million rows) and saves the
# table as matrices in data/matrices/figaro_<year>.rds:
#   Z      intermediate use, 3200 x 3200 (50 areas x 64 industries, rows sell to columns)
#   Y      final demand, 3200 x 250 (columns: 50 buyer countries x 5 categories)
#   W      value-added and tax rows x 3200 industries (D1, D29X39, B2A3G, D21X31, OP_RES, OP_NRES)
#   W_fd   the same rows x 250 final demand columns (taxes on products paid by final users, ...)
#   x      output of each country-sector (row total: sales to industries + final demand)
# Skips years already imported, unless the CSV is newer.
# =============================================================================

source(file.path(root_dir, "config.R"))
library(data.table)

dir.create(matrices_dir, recursive = TRUE, showWarnings = FALSE)

countries <- fread(file.path(labels_dir, "countries.csv"), encoding = "UTF-8")
sectors   <- fread(file.path(labels_dir, "sectors.csv"),   encoding = "UTF-8")

n_c  <- nrow(countries)          # 50 areas
n_s  <- nrow(sectors)            # 64 industries
n    <- n_c * n_s                # 3200 country-sectors
n_k  <- length(fd_categories)    # 5 final demand categories
va_rows <- c("D1", "D29X39", "B2A3G", "D21X31", "OP_RES", "OP_NRES")

# Names of rows/columns: "PL_C24" for country-sectors, "DE_P3_S13" for final demand
cs_names <- paste(rep(countries$code, each = n_s), rep(sectors$code, times = n_c), sep = "_")
fd_names <- paste(rep(countries$code, each = n_k), rep(names(fd_categories), times = n_c), sep = "_")

for (year in years) {
  csv_file <- file.path(figaro_dir, sprintf("flatfile_eu-ic-io_ind-by-ind_%s_%d.csv", edition, year))
  out_file <- file.path(matrices_dir, sprintf("figaro_%d.rds", year))

  if (!file.exists(csv_file)) {
    warning("Missing file: ", csv_file)
    next
  }
  if (file.exists(out_file) && file.mtime(out_file) > file.mtime(csv_file)) next

  raw <- fread(csv_file, select = c("refArea", "rowIi", "counterpartArea", "colIi", "obsValue"),
               showProgress = FALSE)

  # Where each cell goes. Country-sector position = (country - 1) * 64 + industry.
  row_cs <- (match(raw$refArea, countries$code) - 1L) * n_s + match(raw$rowIi, sectors$code)
  row_va <- ifelse(raw$refArea == "W2", match(raw$rowIi, va_rows), NA_integer_)
  col_c  <- match(raw$counterpartArea, countries$code)
  col_cs <- (col_c - 1L) * n_s + match(raw$colIi, sectors$code)
  col_fd <- (col_c - 1L) * n_k + match(raw$colIi, names(fd_categories))
  value  <- raw$obsValue
  value[is.na(value)] <- 0

  # Every cell must land somewhere; otherwise the file has codes we do not know
  unknown <- (is.na(row_cs) & is.na(row_va)) | (is.na(col_cs) & is.na(col_fd))
  if (any(unknown)) {
    stop(year, ": unknown codes, e.g. ",
         paste(head(unique(paste(raw$refArea, raw$rowIi, raw$counterpartArea, raw$colIi)[unknown]), 5),
               collapse = "; "))
  }

  Z    <- matrix(0, n, n,               dimnames = list(cs_names, cs_names))
  Y    <- matrix(0, n, n_c * n_k,       dimnames = list(cs_names, fd_names))
  W    <- matrix(0, length(va_rows), n, dimnames = list(va_rows, cs_names))
  W_fd <- matrix(0, length(va_rows), n_c * n_k, dimnames = list(va_rows, fd_names))

  i <- which(!is.na(row_cs) & !is.na(col_cs)); Z[cbind(row_cs[i], col_cs[i])]    <- value[i]
  i <- which(!is.na(row_cs) & !is.na(col_fd)); Y[cbind(row_cs[i], col_fd[i])]    <- value[i]
  i <- which(!is.na(row_va) & !is.na(col_cs)); W[cbind(row_va[i], col_cs[i])]    <- value[i]
  i <- which(!is.na(row_va) & !is.na(col_fd)); W_fd[cbind(row_va[i], col_fd[i])] <- value[i]
  rm(raw, row_cs, row_va, col_c, col_cs, col_fd, value, i); gc(verbose = FALSE)

  # Output: row total (what each country-sector sells) and column total (what it costs)
  x     <- rowSums(Z) + rowSums(Y)
  x_col <- colSums(Z) + colSums(W)

  # Check: the table is balanced (row total = column total), up to rounding
  gap <- max(abs(x - x_col) / pmax(abs(x), 1))
  if (gap > 1e-3) warning(year, ": row and column totals differ by up to ", signif(100 * gap, 2), "%")

  saveRDS(list(year = year, Z = Z, Y = Y, W = W, W_fd = W_fd, x = x), out_file, compress = FALSE)
  message(year, ": imported (largest row/column gap ", signif(100 * gap, 2), "%)")
}
