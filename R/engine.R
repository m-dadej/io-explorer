# =============================================================================
# Engine: functions that answer "how much of the value added of an ORIGIN depends
# on a SOURCE" with the FIGARO input-output model.
#
# Load it with:
#   root_dir <- "."; source("config.R"); source("R/engine.R")
#
# An origin or a source is chosen by countries and industries. You can pass:
#   countries: country codes ("PL"), group names from config.R ("CEE11", "EU27", "World")
#   sectors:   industry codes ("C29"), broad groups ("Manufacturing"), your groups from
#              config.R, or "All sectors"
#
# Main functions (all values in EUR million, current prices; shares are 0-1):
#   va_by_buyer()          where an origin's value added ends up: by buyer country and
#                          final demand category (households, government, investment, ...)
#   exports_vs_va()        direct (gross) exports vs value added absorbed, by buyer country
#   va_from_final_demand() value added everywhere that depends on a slice of final demand
#   fd_shock()             change in value added everywhere after a final demand shock
#   output_shock()         change in value added everywhere after a production shock in
#                          some country-sectors (supply-chain effect on their suppliers)
#   summarise_impact()     totals of a shock by country, industry, country-sector, ...
#   sources_of()           what an origin depends on most (foreign country-sectors or countries)
#   top_pairs()            the most exposed (origin, source) pairs of country-sectors
#   demand_shares()        share of each country's value added that depends on a final
#                          demand category (e.g. government), at home and abroad
#   domestic_shock()       a shock to a country's own final demand or own production
#   home_vs_abroad()       its effect on home GVA, GVA abroad (leakage) and the home multiplier
#   domestic_content()     where EUR 1 of a country's own final demand ends up (home, abroad, taxes)
#   domestic_multipliers() home GVA and output per EUR 1 of final demand for each home industry
#   home_demand_exposure() share of each home industry's GVA depending on each kind of home demand
#   price_shock()          price changes everywhere after the output price of some country-sectors
#                          rises (cost push, passed on along the supply chain)
#   summarise_price()      a price shock by country, industry, country-sector, ...
#   basket_prices()        price change of what households, government, investors buy, by country
#
# Shocks and the domestic functions take induced = TRUE to add household spending of the extra
# wages, round after round (a "Type II" effect; spend rate in config.R).
#
# The model (for one year):
#   x = output, A = Z / x (input coefficients), L = (I - A)^-1, v = GVA / x
#   value added that final demand y generates:      v * (L y)
#   value added that depends on production of S:    v_R * L[R,S] L[S,S]^-1 x_S
#   (output of S set from outside; R = all other country-sectors)
#   prices (the dual model): p = p A + v; a price change of S set from outside moves the rest by
#   dp_R = dp_S L[S,S]^-1 L[S,R]
# =============================================================================

library(data.table)

# ---- Labels ---------------------------------------------------------------------------
countries <- fread(file.path(labels_dir, "countries.csv"), encoding = "UTF-8")
sectors   <- fread(file.path(labels_dir, "sectors.csv"),   encoding = "UTF-8")

n_c <- nrow(countries); n_s <- nrow(sectors); n <- n_c * n_s; n_k <- length(fd_categories)
country_groups$World <- countries$code
country_names <- setNames(countries$name, countries$code)

sector_groups <- c(
  list("All sectors" = sectors$code),
  split(sectors$code, factor(sectors$group, levels = unique(sectors$group))),
  custom_sector_groups
)
sector_names <- setNames(sectors$name, sectors$code)

# One row per country-sector, in the order of the model's rows and columns
cs <- data.table(
  i            = seq_len(n),
  country      = rep(countries$code, each = n_s),
  sector       = rep(sectors$code, times = n_c),
  sector_name  = rep(sectors$name, times = n_c),
  sector_group = rep(sectors$group, times = n_c)
)
cs[, country_name := country_names[country]]
short_code <- function(code) ifelse(code == "FIGW1", "RoW", code)   # rest of world reads "RoW" in labels
cs[, label := paste(short_code(country), sector_name)]

# One row per final demand column (buyer country x category)
fd <- data.table(
  j        = seq_len(n_c * n_k),
  buyer    = rep(countries$code, each = n_k),
  code     = rep(names(fd_categories), times = n_c),
  category = rep(unname(fd_categories), times = n_c)
)
by_country_fd <- diag(n_c)[rep(seq_len(n_c), each = n_k), ]   # sums final demand columns to buyer countries

# ---- Selections ------------------------------------------------------------------------
expand_countries <- function(x) {
  out <- unlist(lapply(x, function(e) if (e %in% names(country_groups)) country_groups[[e]] else e))
  bad <- setdiff(out, countries$code)
  if (length(bad)) stop("Unknown country or group: ", paste(bad, collapse = ", "))
  unique(out)
}

expand_sectors <- function(x) {
  out <- unlist(lapply(x, function(e) if (e %in% names(sector_groups)) sector_groups[[e]] else e))
  bad <- setdiff(out, sectors$code)
  if (length(bad)) stop("Unknown industry or group: ", paste(bad, collapse = ", "))
  unique(out)
}

# Final demand categories by code ("P3_S13"), name ("Government") or "All"
expand_categories <- function(x) {
  if (identical(x, "All")) return(names(fd_categories))
  out <- ifelse(x %in% fd_categories, names(fd_categories)[match(x, fd_categories)], x)
  bad <- setdiff(out, names(fd_categories))
  if (length(bad)) stop("Unknown final demand category: ", paste(bad, collapse = ", "))
  out
}

# Row numbers of the chosen country-sectors / final demand columns
select_cs <- function(countries = "World", sectors = "All sectors") {
  cs$i[cs$country %in% expand_countries(countries) & cs$sector %in% expand_sectors(sectors)]
}
select_fd <- function(buyers = "World", categories = "All") {
  fd$j[fd$buyer %in% expand_countries(buyers) & fd$code %in% expand_categories(categories)]
}

# A short readable name for a selection, e.g. "PL, Manufacturing"
describe <- function(sel_countries, sel_sectors = "All sectors") {
  c_txt <- paste(ifelse(sel_countries %in% names(country_names), country_names[sel_countries], sel_countries),
                 collapse = " + ")
  s_txt <- paste(ifelse(sel_sectors %in% names(sector_names), sector_names[sel_sectors], sel_sectors),
                 collapse = " + ")
  if (identical(sel_sectors, "All sectors")) c_txt else paste0(c_txt, ", ", s_txt)
}

# ---- Loading (cached in memory) ----------------------------------------------------------
.io_cache <- new.env()

available_years <- function() {
  yrs <- years[file.exists(file.path(model_dir, sprintf("core_%d.rds", years))) &
               file.exists(file.path(model_dir, sprintf("L_%d.rds", years)))]
  if (!length(yrs)) stop("No prepared data. Run prepare_data.R first.")
  yrs
}

cached <- function(key, make) {
  if (is.null(.io_cache[[key]])) .io_cache[[key]] <- make()
  .io_cache[[key]]
}

load_core <- function(year) cached(paste0("core_", year), function() readRDS(file.path(model_dir, sprintf("core_%d.rds", year))))
load_L    <- function(year) cached(paste0("L_", year),    function() readRDS(file.path(model_dir, sprintf("L_%d.rds", year))))

# Input coefficients; only needed for production shocks to very large sets of country-sectors
load_A <- function(year) cached(paste0("A_", year), function() {
  m <- readRDS(file.path(matrices_dir, sprintf("figaro_%d.rds", year)))
  m$Z * rep(ifelse(m$x > 0, 1 / m$x, 0), each = n)
})

clear_cache <- function() rm(list = ls(.io_cache), envir = .io_cache)

# ---- Household spending of extra wages (the optional "induced" effect) ----------------------------
# Pieces of the household loop for one year (cached):
#   wage   compensation of employees per EUR of output, by country-industry
#   LH     output everywhere per EUR of household spending at basic prices, by spending country (n x 50)
#   WLH    wages in each country per EUR of household spending of each country (50 x 50)
#   basic  share of each EUR households spend that buys products at basic prices; the rest is VAT and
#          other taxes on products, and purchases abroad by residents (net of non-residents' purchases)
load_households <- function(year) cached(paste0("hh_", year), function() {
  core <- load_core(year)
  L    <- load_L(year)
  j    <- fd$j[fd$code == "P3_S14"]
  Yh   <- core$Y[, j]
  rest <- colSums(readRDS(file.path(matrices_dir, sprintf("figaro_%d.rds", year)))$W_fd[, j])
  H    <- pmax(Yh, 0)
  H    <- sweep(H, 2, colSums(H), "/")                       # spending mix of each country's households
  wage <- ifelse(core$x > 0, core$va_comp["D1", ] / core$x, 0)
  LH   <- L %*% H
  list(wage = wage, LH = LH, WLH = rowsum(wage * LH, cs$country, reorder = FALSE),
       basic = setNames(colSums(Yh) / (colSums(Yh) + rest), countries$code))
})

# Consumer spending per EUR of extra wages, by country: one value for all, or a named vector
# (countries not named keep household_spend_rate from config.R)
spend_rates <- function(rate = household_spend_rate) {
  out <- setNames(rep(household_spend_rate, n_c), countries$code)
  if (is.null(names(rate))) {
    out[] <- rate[1]
  } else {
    bad <- setdiff(names(rate), countries$code)
    if (length(bad)) stop("Unknown country: ", paste(bad, collapse = ", "))
    out[names(rate)] <- rate
  }
  if (any(is.na(out) | out < 0 | out > 1)) stop("Spending per EUR of wages must be between 0 and 1")
  out
}

# Extra output when households spend part of the extra wages, round after round.
# dx1: output change from the shock and its supply chain (a vector, or a matrix with one column per shock).
#   extra wages by country:  dw = W dx1 + W L H K dw,  so  dw = (I - W L H K)^-1 W dx1
#   extra output:            L H K dw
# W = wages per EUR of output, by country; H = household spending mix of each country;
# K = spending at basic prices per EUR of extra wages (spend rate x basic share).
induced_output <- function(year, dx1, spend_rate = household_spend_rate) {
  hh  <- load_households(year)
  k   <- spend_rates(spend_rate) * hh$basic
  w1  <- rowsum(hh$wage * as.matrix(dx1), cs$country, reorder = FALSE)
  dw  <- solve(diag(n_c) - hh$WLH * rep(k, each = n_c), w1)
  out <- hh$LH %*% (k * dw)
  if (ncol(out) == 1) as.vector(out) else out
}

# ---- 1. Where an origin's value added ends up -----------------------------------------------
# One row per (year, origin, buyer, category). `origin_level` splits the origin:
# "total" (one row per buyer x category), "country", "sector" or "country_sector".
# share = va / origin_va, where origin_va is the origin's whole GVA (all buyers, all categories).
# `domestic` = the buyer is the origin's own country (or one of the origin's countries for "total"/"sector").
va_by_buyer <- function(origin_countries, origin_sectors = "All sectors", years = default_year,
                        origin_level = c("total", "country", "sector", "country_sector")) {
  origin_level <- match.arg(origin_level)
  rows <- select_cs(origin_countries, origin_sectors)
  key  <- switch(origin_level,
    total          = rep("Selection", length(rows)),
    country        = cs$country[rows],
    sector         = cs$sector[rows],
    country_sector = paste(cs$country[rows], cs$sector[rows], sep = "_")
  )
  own <- expand_countries(origin_countries)

  out <- rbindlist(lapply(years, function(yr) {
    core <- load_core(yr)
    agg  <- rowsum(core$VAD[rows, , drop = FALSE], key, reorder = FALSE)
    o_va <- rowsum(core$va[rows], key, reorder = FALSE)[, 1]
    data.table(
      year      = yr,
      origin    = rep(rownames(agg), times = ncol(agg)),
      buyer     = rep(fd$buyer,    each = nrow(agg)),
      category  = rep(fd$category, each = nrow(agg)),
      va        = as.vector(agg),
      origin_va = rep(o_va, times = ncol(agg))
    )
  }))
  out[, share := va / origin_va]
  out[, buyer_name := country_names[buyer]]
  if (origin_level %in% c("country", "country_sector")) {
    out[, domestic := buyer == sub("_.*$", "", origin)]   # "PL" or "PL_C24" -> "PL"
  } else {
    out[, domestic := buyer %in% own]
  }
  out[]
}

# ---- 2. Direct exports vs value added absorbed, by buyer country -----------------------------
# Gross exports count everything shipped to a country (including imported inputs and goods that
# are only processed there and shipped on). Value added absorbed counts the origin's own value
# added that ends up in final demand there, whichever route it takes. Shares are of the foreign total
# (buyers outside the origin's countries).
exports_vs_va <- function(origin_countries, origin_sectors = "All sectors", year = default_year) {
  rows <- select_cs(origin_countries, origin_sectors)
  own  <- expand_countries(origin_countries)
  core <- load_core(year)
  gross <- colSums(core$sales_int[rows, , drop = FALSE] + core$sales_fd[rows, , drop = FALSE])
  va_b  <- colSums(core$VAD[rows, , drop = FALSE]) %*% by_country_fd
  out <- data.table(buyer = countries$code, gross_exports = gross, va_absorbed = as.vector(va_b))
  out <- out[!buyer %in% own]
  out[, `:=`(buyer_name    = country_names[buyer],
             exports_share = gross_exports / sum(gross_exports),
             va_share      = va_absorbed / sum(va_absorbed))]
  out[order(-va_absorbed)]
}

# ---- 3. Shocks ------------------------------------------------------------------------------------
# Both shock functions return an "impact" table: one row per country-sector with its GVA (va),
# the change in value added (dva) and whether it is part of the shocked source (in_source).
# dva is split into four parts that add up to it:
#   dva_direct   the industries the shock hits first: those whose products are bought (final demand
#                shock) or whose output is shocked (production shock)
#   dva_first    first-round suppliers: what the directly hit industries buy as inputs (A x direct)
#   dva_further  further rounds of the supply chain (the suppliers' suppliers, and so on)
#   dva_induced  household spending of extra wages (0 unless induced = TRUE)
# decompose = FALSE skips the split (the three supply-chain parts are NA); it saves loading the input
# coefficients when only the total is needed.
#
# induced = TRUE adds household spending of the extra wages, round after round: households in every
# country spend spend_rate EUR per EUR of extra compensation of employees (see config.R).
new_impact <- function(year, dx, source_rows, shock_size, what, dx_direct = NULL, dx_first = NULL,
                       dx_induced = NULL, spend_rate = NULL) {
  core <- load_core(year)
  ind  <- if (is.null(dx_induced)) numeric(n) else dx_induced
  out  <- copy(cs)
  out[, `:=`(year = year, va = core$va, dva = core$v * (dx + ind), in_source = i %in% source_rows)]
  if (is.null(dx_direct)) {
    out[, c("dva_direct", "dva_first", "dva_further") := NA_real_]
  } else {
    out[, `:=`(dva_direct  = core$v * dx_direct,
               dva_first   = core$v * dx_first,
               dva_further = core$v * (dx - dx_direct - dx_first))]
  }
  out[, dva_induced := core$v * ind]
  setattr(out, "shock", list(what = what, size = shock_size, year = year,
                             induced = !is.null(dx_induced), spend_rate = spend_rate))
  out[]
}

# Final demand shock. Choose which final demand: buyer countries x categories x product
# (the country-sectors that make what is bought). Give the size as a % change of that demand
# (pct = -10) or as an amount in EUR million (eur_m = -1000), spread like current demand.
fd_shock <- function(year = default_year, buyers = "World", categories = "All",
                     product_countries = "World", product_sectors = "All sectors",
                     pct = NULL, eur_m = NULL, induced = FALSE, spend_rate = household_spend_rate,
                     decompose = TRUE) {
  if (is.null(pct) == is.null(eur_m)) stop("Give either pct or eur_m")
  core <- load_core(year)
  L    <- load_L(year)
  rows <- select_cs(product_countries, product_sectors)
  cols <- select_fd(buyers, categories)
  Ysub <- core$Y[rows, cols, drop = FALSE]
  if (!is.null(pct)) {
    dy <- rowSums(Ysub) * pct / 100
  } else {
    w <- rowSums(pmax(Ysub, 0))   # spread like current demand (inventory decreases left out)
    if (sum(w) <= 0) stop("There is no final demand in this selection")
    dy <- eur_m * w / sum(w)
  }
  dx <- as.vector(L[, rows, drop = FALSE] %*% dy)
  direct <- first <- NULL
  if (decompose) {
    direct <- numeric(n); direct[rows] <- dy                          # the producers of what is bought
    first  <- as.vector(load_A(year)[, rows, drop = FALSE] %*% dy)    # their purchases of inputs
  }
  new_impact(year, dx, rows, sum(dy), "final demand", direct, first,
             if (induced) induced_output(year, dx, spend_rate), if (induced) spend_rate)
}

# Value added everywhere that depends on a slice of final demand (= a +100% final demand shock)
va_from_final_demand <- function(year = default_year, buyers = "World", categories = "All",
                                 product_countries = "World", product_sectors = "All sectors") {
  fd_shock(year, buyers, categories, product_countries, product_sectors, pct = 100, decompose = FALSE)
}

# Output change in rows `rows` when the output of `S` changes by dxS (S set from outside the model)
output_change <- function(year, S, dxS, rows) {
  R <- setdiff(seq_len(n), S)
  dx <- numeric(n)
  dx[S] <- dxS
  if (!length(R)) return(dx[rows])
  if (length(S) <= length(R)) {
    L <- load_L(year)
    R_needed <- intersect(rows, R)
    dx[R_needed] <- L[R_needed, S, drop = FALSE] %*% solve(L[S, S, drop = FALSE], dxS)
  } else {
    A <- load_A(year)   # large S: solving for the (smaller) rest is faster
    dx[R] <- solve(diag(length(R)) - A[R, R, drop = FALSE], A[R, S, drop = FALSE] %*% dxS)
  }
  dx[rows]
}

# Production shock: the output of the source country-sectors changes by pct % (or by eur_m in total,
# spread by their output). Their suppliers (at home and abroad, directly and through the supply chain)
# lose the sales. The source's own value added changes by the same %. With induced = TRUE, household
# spending of the extra (or lost) wages comes on top, for all industries, the source included.
output_shock <- function(year = default_year, source_countries, source_sectors = "All sectors",
                         pct = NULL, eur_m = NULL, induced = FALSE, spend_rate = household_spend_rate,
                         decompose = TRUE) {
  if (is.null(pct) == is.null(eur_m)) stop("Give either pct or eur_m")
  core <- load_core(year)
  S <- select_cs(source_countries, source_sectors)
  S <- S[core$x[S] > 0]
  if (!length(S)) stop("The source has no output")
  dxS <- if (!is.null(pct)) core$x[S] * pct / 100 else eur_m * core$x[S] / sum(core$x[S])
  dx <- output_change(year, S, dxS, seq_len(n))
  direct <- first <- NULL
  if (decompose) {
    direct <- numeric(n); direct[S] <- dxS                             # the shocked producers themselves
    first  <- as.vector(load_A(year)[, S, drop = FALSE] %*% dxS)       # their purchases of inputs
    first[S] <- 0   # what they buy from each other is part of their (fixed) output, not extra output
  }
  new_impact(year, dx, S, sum(dxS), "production", direct, first,
             if (induced) induced_output(year, dx, spend_rate), if (induced) spend_rate)
}

# Totals of an impact table. level: "country", "country_sector", "sector" (industry, summed over
# the countries kept), "sector_group", "country_sector_group", "total". `countries` keeps only these
# origins. min_va leaves out rows with less GVA (EUR m), except at country and total level.
#   share_va         = change as a share of the row's own GVA
#   share_country_va = change as a share of the country's whole GVA
#   dva_excl_source  = change without the shocked source itself (only the spill-over to others)
#   dva_direct, dva_first, dva_further, dva_induced = the four parts of the change (see new_impact())
#   dva_induced_excl_source = household spending part without the shocked source. (Outside the
#                      source the direct part is 0; in a production shock the source has no
#                      first-round or further part, so dva_excl_source = first + further + this.)
summarise_impact <- function(impact, level = c("country", "country_sector", "sector", "sector_group",
                                               "country_sector_group", "total"),
                             countries = "World", min_va = 0) {
  level <- match.arg(level)
  keep <- expand_countries(countries)
  dt <- impact[country %in% keep]
  country_va <- dt[, .(country_va = sum(va)), by = country]
  by <- switch(level,
    country              = "country",
    country_sector       = c("country", "sector"),
    sector               = "sector",
    sector_group         = "sector_group",
    country_sector_group = c("country", "sector_group"),
    total                = character(0)
  )
  out <- dt[, .(va = sum(va), dva = sum(dva), dva_excl_source = sum(dva[!in_source]),
                dva_direct = sum(dva_direct), dva_first = sum(dva_first), dva_further = sum(dva_further),
                dva_induced = sum(dva_induced), dva_induced_excl_source = sum(dva_induced[!in_source]),
                in_source = all(in_source)), by = by]
  out[, share_va := dva / va]
  if ("country" %in% by) {
    out[country_va, country_va := i.country_va, on = "country"]
    out[, share_country_va := dva / country_va]
    out[, country_name := country_names[country]]
  }
  if ("sector" %in% by) out[, sector_name := sector_names[sector]]
  if (level == "country_sector") out[, label := paste(short_code(country), sector_name)]
  if (!level %in% c("country", "total")) out <- out[va >= min_va]
  out[order(dva)][]
}

# ---- 4. Dependence between country-sectors --------------------------------------------------------
# Share of the GVA of each origin (rows) that depends on each source (columns), for one year.
#   type "production":   on the source's production (it buys inputs from the origin, directly or
#                        through others). D[r,s] = L[r,s] x_s / (L[s,s] x_r). A -10% production shock
#                        in s lowers r's value added by 10% x D[r,s]. Not additive across sources.
#   type "final_demand": on final demand for the source's products, all buyers.
#                        D[r,s] = L[r,s] y_s / x_r. Additive across sources (sums to 1 over all s).
dependence <- function(year, origins, sources, type = c("production", "final_demand")) {
  type <- match.arg(type)
  core <- load_core(year)
  L    <- load_L(year)
  inv_x <- ifelse(core$x[origins] > 0, 1 / core$x[origins], NA_real_)
  w <- if (type == "production") core$x[sources] / L[cbind(sources, sources)] else rowSums(core$Y)[sources]
  D <- L[origins, sources, drop = FALSE] * inv_x
  D <- D * rep(w, each = length(origins))
  if (type == "production") D[outer(origins, sources, "==")] <- NA   # not "dependent on itself"
  D
}

# The most exposed (origin, source) pairs. By default: CEE country-sectors (with GVA of at least
# min_va) and the single foreign country-sector they depend on most.
top_pairs <- function(year = default_year, origin_countries = default_focus, origin_sectors = "All sectors",
                      source_countries = "World", source_sectors = "All sectors",
                      type = c("production", "final_demand"), foreign_only = TRUE,
                      min_va = min_va_eur_m, n_top = 50) {
  type <- match.arg(type)
  core <- load_core(year)
  O <- select_cs(origin_countries, origin_sectors)
  O <- O[core$va[O] >= min_va]
  S <- select_cs(source_countries, source_sectors)
  if (!length(O) || !length(S)) return(data.table())
  D <- dependence(year, O, S, type)
  if (foreign_only) D[outer(cs$country[O], cs$country[S], "==")] <- NA
  top <- head(order(D, decreasing = TRUE, na.last = NA), n_top)
  ij  <- arrayInd(top, dim(D))
  r <- O[ij[, 1]]; s <- S[ij[, 2]]
  data.table(
    year          = year,
    origin        = cs$label[r], origin_country = cs$country[r], origin_sector = cs$sector[r],
    source        = cs$label[s], source_country = cs$country[s], source_sector = cs$sector[s],
    share         = D[top],
    va_dependent  = D[top] * core$va[r],
    origin_va     = core$va[r]
  )
}

# What an origin depends on most. Returns one row per source with the share of the origin's GVA
# that depends on it.
#   source_level "country_sector": each single country-sector
#   source_level "country":        each whole country (production of all its industries, or final
#                                  demand for all its products)
#   source_level "sector":         each industry, all source countries together (final_demand only;
#                                  for production, choose the industry as a source in output_shock())
# foreign_only leaves out sources in the origin's own countries.
sources_of <- function(year = default_year, origin_countries, origin_sectors = "All sectors",
                       type = c("production", "final_demand"),
                       source_level = c("country_sector", "country", "sector"),
                       source_countries = "World", foreign_only = TRUE) {
  type <- match.arg(type); source_level <- match.arg(source_level)
  core <- load_core(year)
  L    <- load_L(year)
  O    <- select_cs(origin_countries, origin_sectors)
  va_O <- sum(core$va[O])
  own  <- unique(cs$country[O])
  keep <- cs$country %in% expand_countries(source_countries) & !(foreign_only & cs$country %in% own)

  if (type == "production" && source_level == "sector") {
    stop("For production, industries across all countries are not available here: use output_shock()")
  }

  if (type == "production" && source_level == "country") {
    src <- setdiff(intersect(expand_countries(source_countries), countries$code), own)
    res <- rbindlist(lapply(src, function(cc) {
      S <- select_cs(cc); S <- S[core$x[S] > 0]
      dx <- output_change(year, S, -core$x[S], O)
      data.table(source = cc, source_name = country_names[[cc]], va_dependent = -sum(core$v[O] * dx))
    }))
  } else {
    vO_L <- colSums(core$v[O] * L[O, , drop = FALSE])   # GVA of the origin per EUR of final demand for each product
    contrib <- if (type == "production") vO_L * core$x / diag(L) else vO_L * rowSums(core$Y)
    if (type == "production") contrib[O] <- NA          # an origin does not "depend on itself"
    dt <- data.table(cs, va_dependent = contrib)[keep & !is.na(va_dependent)]
    res <- switch(source_level,
      country_sector = dt[, .(source = paste(country, sector, sep = "_"), source_name = label, va_dependent)],
      country        = dt[, .(va_dependent = sum(va_dependent)), by = .(source = country)][
                         , source_name := country_names[source]],
      sector         = dt[, .(va_dependent = sum(va_dependent)), by = .(source = sector)][
                         , source_name := sector_names[source]]
    )
  }
  res[, `:=`(year = year, share = va_dependent / va_O, origin_va = va_O)]
  setcolorder(res, c("year", "source", "source_name", "share", "va_dependent", "origin_va"))
  res[order(-share)][]
}

# ---- 5. Share of value added that depends on a final demand category ------------------------------
# For each origin country: share of its GVA absorbed by the chosen category (default: government
# consumption) at home and abroad.
demand_shares <- function(years = default_year, origin_countries = "World", categories = "P3_S13") {
  cats <- expand_categories(categories)
  own  <- expand_countries(origin_countries)
  rbindlist(lapply(years, function(yr) {
    core <- load_core(yr)
    by_c <- rowsum(core$VAD, cs$country, reorder = FALSE)            # 50 origin countries x 250
    va_c <- rowsum(core$va, cs$country, reorder = FALSE)[, 1]
    cols <- fd$code %in% cats
    rbindlist(lapply(own, function(cc) {
      dom <- sum(by_c[cc, cols & fd$buyer == cc])
      frn <- sum(by_c[cc, cols & fd$buyer != cc])
      data.table(year = yr, country = cc, country_name = country_names[[cc]], va = va_c[[cc]],
                 va_domestic = dom, va_foreign = frn,
                 share_domestic = dom / va_c[[cc]], share_foreign = frn / va_c[[cc]])
    }))
  }))
}

# ---- 6. The home economy: shocks inside one country -------------------------------------------------
# A shock to a country's own final demand or own production: what it does to the country's own
# industries, and how much of it leaks abroad (GVA created in other countries through imports).

# type "final_demand": the country's own buyers change their final demand (the chosen categories,
#   products of `sectors`). products_from "all" changes home-made and imported products alike (buyers
#   keep their import shares, so part of the shock leaks abroad at once); "home" changes only demand
#   for products made in the country.
# type "production": the output of the country's own industries `sectors` changes; their suppliers,
#   at home and abroad, follow.
# induced = TRUE adds household spending of the extra wages (see fd_shock()).
domestic_shock <- function(year = default_year, country, type = c("final_demand", "production"),
                           categories = "All", sectors = "All sectors", products_from = c("all", "home"),
                           pct = NULL, eur_m = NULL, induced = FALSE, spend_rate = household_spend_rate,
                           decompose = TRUE) {
  type <- match.arg(type); products_from <- match.arg(products_from)
  if (length(country) != 1 || !country %in% countries$code) stop("Give one country code, e.g. \"PL\"")
  if (type == "final_demand") {
    fd_shock(year, buyers = country, categories = categories,
             product_countries = if (products_from == "home") country else "World",
             product_sectors = sectors, pct = pct, eur_m = eur_m, induced = induced, spend_rate = spend_rate,
             decompose = decompose)
  } else {
    output_shock(year, country, sectors, pct = pct, eur_m = eur_m, induced = induced, spend_rate = spend_rate,
                 decompose = decompose)
  }
}

# Headline numbers of a shock, seen from one country:
#   dva_home           change in the country's GVA
#   dva_home_other     of which outside the shocked industries (production shock) or outside the
#                      industries whose products are bought (final demand shock)
#   dva_home_direct, dva_home_first, dva_home_further, dva_home_induced
#                      the change in home GVA split into direct, first-round suppliers, further
#                      supply-chain rounds and household spending (see new_impact())
#   dva_abroad         change in GVA in all other countries (leakage through imports)
#   home_per_eur       change in home GVA per EUR 1 of shock (the home GVA multiplier)
#   home_per_eur_type1 the same without household spending (supply chain only)
#   leakage            share of the change in GVA everywhere that happens abroad
home_vs_abroad <- function(impact, home) {
  at_home <- impact$country == home
  out <- data.table(
    shock            = attr(impact, "shock")$size,
    dva_home         = sum(impact$dva[at_home]),
    dva_home_other   = sum(impact$dva[at_home & !impact$in_source]),
    dva_home_direct  = sum(impact$dva_direct[at_home]),
    dva_home_first   = sum(impact$dva_first[at_home]),
    dva_home_further = sum(impact$dva_further[at_home]),
    dva_home_induced = sum(impact$dva_induced[at_home]),
    dva_abroad       = sum(impact$dva[!at_home]),
    home_va          = sum(impact$va[at_home])
  )
  out[, `:=`(share_home_va      = dva_home / home_va,
             home_per_eur       = dva_home / shock,
             home_per_eur_type1 = (dva_home - dva_home_induced) / shock,
             abroad_per_eur     = dva_abroad / shock,
             leakage            = dva_abroad / (dva_home + dva_abroad))]
  out[]
}

# Where EUR 1 of the country's own final demand ends up, by category: GVA at home, GVA abroad, and
# the rest (taxes less subsidies on products paid on inputs along the chain, and two small FIGARO
# adjustment rows). The three add up to 1. Final demand is at basic prices and includes imported
# final goods. Over all products, so it equals the home GVA per EUR of a shock to a whole category.
# induced = TRUE adds two columns: va_home_hh, the extra home GVA from household spending of the
# extra wages (EUR m), and home_hh_per_eur, the same per EUR of final demand (on top of the EUR 1).
domestic_content <- function(years = default_year, country, induced = FALSE,
                             spend_rate = household_spend_rate) {
  at_home <- cs$country == country
  cols <- fd$j[fd$buyer == country]
  rbindlist(lapply(years, function(yr) {
    core <- load_core(yr)
    out <- data.table(year = yr, code = fd$code[cols], category = fd$category[cols],
                      final_demand = colSums(core$Y[, cols]),
                      va_home      = colSums(core$VAD[at_home, cols]),
                      va_abroad    = colSums(core$VAD[!at_home, cols]))
    out <- rbind(out, out[, .(year = yr, code = "ALL", category = "All final demand",
                              final_demand = sum(final_demand), va_home = sum(va_home),
                              va_abroad = sum(va_abroad))])
    out[, `:=`(home_per_eur   = va_home / final_demand,
               abroad_per_eur = va_abroad / final_demand,
               other_per_eur  = 1 - (va_home + va_abroad) / final_demand)]
    if (induced) {
      Ysel <- core$Y[, cols]
      ind  <- induced_output(yr, load_L(yr) %*% cbind(Ysel, rowSums(Ysel)), spend_rate)
      out[, va_home_hh := colSums(core$v[at_home] * ind[at_home, , drop = FALSE])]
      out[, home_hh_per_eur := va_home_hh / final_demand]
    }
    out
  }))
}

# Home multipliers by industry: what EUR 1 of final demand for a home industry's products (whoever
# buys them) generates
#   output_multiplier  output of all home industries (EUR)
#   va_own             GVA of the industry itself
#   va_other_home      GVA of the other home industries (its home supply chain)
#   va_home            all home GVA (va_own + va_other_home)
#   va_abroad          GVA abroad (imported inputs, direct and indirect)
#   other              taxes less subsidies on products in the inputs, and small adjustments
#   va_home_hh         (induced = TRUE only) extra home GVA from household spending of the extra
#                      wages, on top of the EUR 1
domestic_multipliers <- function(year = default_year, country, induced = FALSE,
                                 spend_rate = household_spend_rate) {
  core <- load_core(year)
  L    <- load_L(year)
  S    <- select_cs(country)
  at_home <- cs$country == country
  vL <- core$v * L[, S]                    # GVA per EUR of final demand for each home product (3200 x 64)
  out <- cs[S, .(country, sector, sector_name, sector_group, label)]
  out[, `:=`(year              = year,
             va                = core$va[S],
             final_demand      = rowSums(core$Y[S, , drop = FALSE]),
             output_multiplier = colSums(L[at_home, S, drop = FALSE]),
             va_own            = vL[cbind(S, seq_along(S))],
             va_home           = colSums(vL[at_home, , drop = FALSE]),
             va_abroad         = colSums(vL[!at_home, , drop = FALSE]))]
  out[, `:=`(va_other_home = va_home - va_own, other = 1 - va_home - va_abroad)]
  na_cols <- c("output_multiplier", "va_own", "va_home", "va_abroad", "va_other_home", "other")
  if (induced) {
    ind <- induced_output(year, L[, S], spend_rate)
    out[, va_home_hh := colSums(core$v[at_home] * ind[at_home, , drop = FALSE])]
    na_cols <- c(na_cols, "va_home_hh")
  }
  out[core$x[S] <= 0, (na_cols) := NA_real_]
  out[]
}

# Share of each home industry's (or industry group's) GVA that depends on each category of the
# country's own final demand, and on foreign final demand (all categories together). Each row adds
# up to 100%. A -10% change in a category of home final demand (all products, home-made and imported)
# lowers the industry's GVA by 10% x its share.
home_demand_exposure <- function(year = default_year, country, level = c("sector", "sector_group")) {
  level <- match.arg(level)
  core <- load_core(year)
  rows <- select_cs(country)
  key  <- if (level == "sector") cs$sector[rows] else cs$sector_group[rows]
  V    <- rowsum(core$VAD[rows, , drop = FALSE], key, reorder = FALSE)
  va   <- rowsum(core$va[rows], key, reorder = FALSE)[, 1]
  home <- fd$buyer == country
  parts <- c(lapply(names(fd_categories), function(k) home & fd$code == k), list(!home))
  out <- rbindlist(Map(function(cols, code, demand) {
    data.table(origin = rownames(V), va = va, code = code, demand = demand,
               va_dependent = rowSums(V[, cols, drop = FALSE]))
  }, parts, c(names(fd_categories), "FOREIGN"), c(unname(fd_categories), "Foreign final demand")))
  out[, `:=`(year = year, share = va_dependent / va,
             origin_name = if (level == "sector") sector_names[origin] else origin)]
  out[]
}

# ---- 7. Price shocks (cost push) --------------------------------------------------------------------
# The price model, the dual of the quantity model above: the price of each country-sector's output
# is the cost of its inputs plus its GVA per EUR of output, p = p A + v (all prices are 1 in the
# table's year). The output price of the source country-sectors S rises by pct %, set from outside
# the model (e.g. a world oil price). Every buyer passes its extra input costs on in full, along the
# whole supply chain, at home and abroad. GVA per EUR of output stays as it is everywhere else (no
# change in wages or margins per unit), volumes stay as they are, and nobody switches inputs.
#   dp_R = dp_S L[S,S]^-1 L[S,R]  (= dp_S A[S,R] (I - A[R,R])^-1), R = all other country-sectors
# Prices are basic prices: before VAT, excise and other taxes on products.
#
# Returns one row per country-sector (shares: 0.01 = +1%):
#   dp           price change of its output. Outside S: its extra input cost per EUR of output.
#                In S: the shock itself
#   dp_direct    extra cost of what it buys directly from S, per EUR of output: the cost push if
#                nobody else passed anything on (the lower end; dp is the upper end)
#   dp_indirect  extra cost per EUR of output that comes through other suppliers (with dp_direct it
#                adds up to the extra input cost per EUR; outside S that is dp)
#   cost, cost_direct  the same in EUR m at today's volumes (per EUR x output)
#   cost_share_va      cost / GVA: the squeeze on the margin if the industry cannot pass it on
#   x            output; zero or negative output counts as no output
#   in_source    whether it is one of the producers whose price is set
price_shock <- function(year = default_year, source_countries, source_sectors = "All sectors", pct = 10) {
  if (!is.numeric(pct) || length(pct) != 1 || is.na(pct) || pct == 0) stop("Give a price change other than 0")
  core <- load_core(year)
  A    <- load_A(year)
  S <- select_cs(source_countries, source_sectors)
  S <- S[core$x[S] > 0]
  if (!length(S)) stop("The source has no output")
  R  <- setdiff(seq_len(n), S)
  dS <- rep(pct / 100, length(S))
  dp <- numeric(n)
  dp[S] <- dS
  if (length(R)) {
    if (length(S) <= length(R)) {
      L <- load_L(year)
      dp[R] <- as.vector(crossprod(L[S, R, drop = FALSE], solve(t(L[S, S, drop = FALSE]), dS)))
    } else {   # large S: solving for the (smaller) rest is faster
      dp[R] <- solve(t(diag(length(R)) - A[R, R, drop = FALSE]), as.vector(crossprod(A[S, R, drop = FALSE], dS)))
    }
  }
  unit_cost <- as.vector(crossprod(A, dp))                       # extra input cost per EUR of output
  direct    <- as.vector(crossprod(A[S, , drop = FALSE], dS))    # of which: bought directly from S
  x <- pmax(core$x, 0)
  out <- copy(cs)
  out[, `:=`(year = year, x = x, va = core$va, dp = dp, dp_direct = direct, dp_indirect = unit_cost - direct,
             cost = unit_cost * x, cost_direct = direct * x, in_source = i %in% S)]
  out[, cost_share_va := cost / va]
  setattr(out, "shock", list(what = "price", size = pct, year = year, n_source = length(S),
                             source_output = sum(x[S])))
  out[]
}

# Totals of a price shock. level as in summarise_impact(); `countries` keeps only these. The producers
# whose price is set are left out (their price is the shock itself). Prices are averaged with output
# weights, i.e. the price index of what the rows produce:
#   dp, dp_direct, dp_indirect  price change (0.01 = +1%) and its two parts
#   cost, cost_direct           extra input cost, EUR m at today's volumes
#   cost_share_va               cost / GVA
# min_va leaves out rows with less GVA (EUR m), except at country and total level.
summarise_price <- function(price, level = c("country", "country_sector", "sector", "sector_group",
                                             "country_sector_group", "total"),
                            countries = "World", min_va = 0) {
  level <- match.arg(level)
  dt <- price[country %in% expand_countries(countries) & in_source == FALSE]
  by <- switch(level,
    country              = "country",
    country_sector       = c("country", "sector"),
    sector               = "sector",
    sector_group         = "sector_group",
    country_sector_group = c("country", "sector_group"),
    total                = character(0)
  )
  out <- dt[, .(x = sum(x), va = sum(va), cost = sum(cost), cost_direct = sum(cost_direct)), by = by]
  out[, `:=`(dp = ifelse(x > 0, cost / x, NA_real_), dp_direct = ifelse(x > 0, cost_direct / x, NA_real_))]
  out[, `:=`(dp_indirect = dp - dp_direct, cost_share_va = cost / va)]
  if ("country" %in% by) out[, country_name := country_names[country]]
  if ("sector" %in% by) out[, sector_name := sector_names[sector]]
  if (level == "country_sector") out[, label := paste(short_code(country), sector_name)]
  if (!level %in% c("country", "total")) out <- out[va >= min_va]
  out[order(-dp)][]
}

# Price change of what final buyers buy, by buyer country and final demand category, at basic prices:
# the price changes weighted by the basket of each buyer country (home-made and imported products).
#   dp          price change of the basket (0.01 = +1%)
#   dp_source   of which products of the producers whose price is set, bought directly
#   dp_other    other products, whose prices rise through their costs
#   final_demand  the basket, EUR m (decreases of inventories left out)
basket_prices <- function(price, buyers = "World", categories = c("P3_S14", "P3_S13", "P51G")) {
  core <- load_core(attr(price, "shock")$year)
  cols <- select_fd(buyers, categories)
  Yp   <- pmax(core$Y[, cols, drop = FALSE], 0)
  src  <- price$in_source
  out <- data.table(buyer = fd$buyer[cols], code = fd$code[cols], category = fd$category[cols],
                    final_demand = colSums(Yp),
                    dp           = colSums(price$dp * Yp),
                    dp_source    = colSums(price$dp[src] * Yp[src, , drop = FALSE]))
  out[, `:=`(dp = dp / final_demand, dp_source = dp_source / final_demand)]
  out[, `:=`(dp_other = dp - dp_source, buyer_name = country_names[buyer])]
  setcolorder(out, c("buyer", "buyer_name"))
  out[]
}

# ---- Several years at once --------------------------------------------------------------------------
# Runs f(year, ...) for each year and stacks the results, e.g.
#   over_years(2015:2024, function(y) summarise_impact(output_shock(y, "DE", "C29", pct = -10), "country", "CEE11"))
over_years <- function(years, f, ...) {
  rbindlist(lapply(years, function(y) { r <- as.data.table(f(y, ...)); r[, year := y]; r }), fill = TRUE)
}
