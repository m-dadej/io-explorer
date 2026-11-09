# =============================================================================
# Worked examples: typical questions answered with the engine
# and checks that the model behaves as it should.
# Run from the project folder (open io-explorer.Rproj, then source this file).
# =============================================================================

root_dir <- "."
source("config.R")
source("R/engine.R")

yr <- max(available_years())

# ---- Checks ----------------------------------------------------------------------------------
check <- function(ok, what) {
  if (!isTRUE(ok)) stop("CHECK FAILED: ", what)
  message("ok: ", what)
}
core <- load_core(yr)

check(max(abs(rowSums(core$VAD) - core$va)[core$x > 0]) < 0.01,
      "every country-industry's GVA is fully allocated to buyers")

# (Tiny country-industries whose final demand is negative - stock drawdowns larger than their
#  output - can show shares above 1; rankings leave them out with the GVA threshold, and so does this check.)
O <- select_cs("CEE11"); O <- O[core$va[O] >= min_va_eur_m]
D <- dependence(yr, O, select_cs("World"), "production")
check(all(D >= -1e-9 & D <= 1 + 1e-9, na.rm = TRUE), "dependence shares lie between 0 and 1")
check(all(is.na(diag(dependence(yr, 1:50, 1:50)))), "a country-industry does not depend on itself")

a <- output_shock(yr, "DE", "C29", pct = -10)
b <- output_shock(yr, "DE", "C29", pct = -100)
check(isTRUE(all.equal(10 * a$dva, b$dva)), "shocks are linear (-10% x 10 = -100%)")

d_pair <- dependence(yr, select_cs("PL", "C24"), select_cs("DE", "C29"))
check(abs(-b[country == "PL" & sector == "C24", dva / va] - d_pair) < 1e-10,
      "pair dependence equals a -100% production shock")

us <- fd_shock(yr, buyers = "US", pct = -100)
vb <- va_by_buyer("World", years = yr, origin_level = "country_sector")[buyer == "US", .(va = sum(va)), by = origin]
check(isTRUE(all.equal(-us$dva, vb$va[match(paste(us$country, us$sector, sep = "_"), vb$origin)], tolerance = 1e-8)),
      "-100% of US final demand = GVA absorbed by US buyers")

fd_all <- sources_of(yr, "PL", type = "final_demand", source_level = "country", foreign_only = FALSE)
check(abs(sum(fd_all$share) - 1) < 1e-8, "final demand shares of an origin add up to 100%")

# Production shock to a very large set (everything outside Poland) uses the other formula: compare both
S <- select_cs(setdiff(countries$code, "PL")); S <- S[core$x[S] > 0]
via_A <- output_change(yr, S, -core$x[S] * 0.01, select_cs("PL"))
L <- load_L(yr); R <- select_cs("PL")
via_L <- as.vector(L[R, S] %*% solve(L[S, S], -core$x[S] * 0.01))
check(isTRUE(all.equal(via_A, via_L, tolerance = 1e-6)), "both production-shock formulas agree")

# Domestic shock functions
pl_gov <- home_vs_abroad(domestic_shock(yr, "PL", categories = "Government", pct = 10), "PL")
pl_content <- domestic_content(yr, "PL")
check(abs(pl_gov$home_per_eur - pl_content[code == "P3_S13", home_per_eur]) < 1e-10,
      "home GVA per EUR of a government shock = home content of government consumption")
check(all(pl_content[code != "P5M", other_per_eur > 0 & other_per_eur < 0.1]),
      "taxes on products & other take a small, positive part of each EUR")

pl_mult <- domestic_multipliers(yr, "PL")
cars <- va_from_final_demand(yr, product_countries = "PL", product_sectors = "C29")
check(abs(pl_mult[sector == "C29", va_home * final_demand] - cars[country == "PL", sum(dva)]) < 1e-6,
      "home multiplier x final demand = home GVA from final demand for the industry's products")

expo <- home_demand_exposure(yr, "PL")
check(max(abs(expo[, sum(share), by = origin]$V1 - 1), na.rm = TRUE) < 1e-8,
      "each home industry's GVA is fully split between home demand categories and foreign demand")

# Household spending of extra wages (induced effect)
g0 <- domestic_shock(yr, "PL", categories = "Government", eur_m = 1000)
g2 <- domestic_shock(yr, "PL", categories = "Government", eur_m = 1000, induced = TRUE)
check(isTRUE(all.equal(g0$dva, domestic_shock(yr, "PL", categories = "Government", eur_m = 1000,
                                              induced = TRUE, spend_rate = 0)$dva)),
      "household spending with a spend rate of 0 = no household spending")
# (A few loss-making country-industries have negative GVA: more output lowers their GVA, so they are
#  left out of the first part.)
check(all(g2$dva_induced[core$v >= 0] >= -1e-12) && isTRUE(all.equal(g2$dva - g2$dva_induced, g0$dva)),
      "household spending only adds to a positive shock; the supply-chain part is unchanged")

hh <- load_households(yr)
k <- spend_rates(household_spend_rate) * hh$basic
dx1 <- as.vector(load_L(yr) %*% (core$Y[, fd$j[fd$buyer == "PL" & fd$code == "P3_S13"]] / 100))
rounds <- numeric(n); r <- dx1
for (i in 1:60) {                       # spend the wages round after round
  r <- as.vector(hh$LH %*% (k * rowsum(hh$wage * r, cs$country, reorder = FALSE)[, 1]))
  rounds <- rounds + r
}
check(max(abs(rounds - induced_output(yr, dx1))) < 1e-8, "the household loop equals spending round after round")

pl_mult_hh <- domestic_multipliers(yr, "PL", induced = TRUE)
cars_hh <- domestic_shock(yr, "PL", sectors = "C29", products_from = "home", pct = 10, induced = TRUE)
check(abs(home_vs_abroad(cars_hh, "PL")$home_per_eur - pl_mult_hh[sector == "C29", va_home + va_home_hh]) < 1e-10,
      "home multiplier with household spending = home GVA per EUR of a shock to the industry's products")

# Parts of the effect: direct, first-round suppliers, further rounds, household spending
pf <- domestic_shock(yr, "PL", type = "production", sectors = "F", eur_m = 30, induced = TRUE)
check(isTRUE(all.equal(pf$dva, pf$dva_direct + pf$dva_first + pf$dva_further + pf$dva_induced)) &&
        isTRUE(all.equal(g2$dva, g2$dva_direct + g2$dva_first + g2$dva_further + g2$dva_induced)),
      "the four parts add up to the total change (production and final demand shocks)")
A <- load_A(yr); f <- select_cs("PL", "F"); g46 <- select_cs("PL", "G46")
check(abs(pf$dva_first[g46] - core$v[g46] * A[g46, f] * 30) < 1e-10 && abs(pf$dva_direct[f] - core$v[f] * 30) < 1e-10,
      "first-round part = GVA of the inputs the shocked industry buys directly; direct part = its own GVA")
check(isTRUE(all.equal(pf$dva, domestic_shock(yr, "PL", type = "production", sectors = "F", eur_m = 30,
                                              induced = TRUE, decompose = FALSE)$dva)),
      "splitting into parts does not change the total")

# Price shocks (the price model, p = p A + v)
pq <- price_shock(yr, "World", "C19", pct = 10)
check(max(abs(as.vector(crossprod(A, pq$dp)) - pq$dp)[!pq$in_source]) < 1e-12,
      "price shock: every other price rises by its extra input cost per EUR")
L <- load_L(yr); jq <- select_cs("PL", "D35")
check(max(abs(price_shock(yr, "PL", "D35", pct = 10)$dp - 0.1 * L[jq, ] / L[jq, jq])) < 1e-12,
      "one producer's price rise moves the others by shock x L[j, ] / L[j, j]")
check(isTRUE(all.equal(price_shock(yr, "World", "C19", pct = 20)$dp, 2 * pq$dp)), "price shocks are linear")
big <- price_shock(yr, setdiff(countries$code, "PL"), pct = 1)    # a large set uses the other formula
Sq <- which(big$in_source); Rq <- which(!big$in_source)
check(max(abs(big$dp[Rq] - as.vector(crossprod(L[Sq, Rq], solve(t(L[Sq, Sq]), rep(0.01, length(Sq))))))) < 1e-10,
      "both price-shock formulas agree")
yh <- pmax(core$Y[, fd$j[fd$buyer == "PL" & fd$code == "P3_S14"]], 0)
check(abs(basket_prices(pq, "PL", "Households")$dp_source - 0.1 * sum(yh[pq$in_source]) / sum(yh)) < 1e-12,
      "household basket: the producers' own products count with the full rise")

# ---- Q1. Where does Polish GVA end up? ---------------------------------------------------------
pl <- va_by_buyer("PL", years = yr)
pl[, .(share = sum(share)), by = .(buyer_name, domestic)][order(-share)][1:10]

# Gross exports vs value added absorbed: the US takes more Polish GVA than direct exports suggest
exports_vs_va("PL", year = yr)[1:10, .(buyer_name, exports_share, va_share)]

# ---- Q2. Which CEE country-industries depend most on German car production? ---------------------
dependence_on_de_cars <- summarise_impact(output_shock(yr, "DE", "C29", pct = -100),
                                          "country_sector", "CEE11", min_va = min_va_eur_m)
dependence_on_de_cars[in_source == FALSE][order(share_va)][1:15,
  .(label, share_dependent = -share_va, gva_dependent = -dva)]

# The same, per country (share of the whole economy)
summarise_impact(output_shock(yr, "DE", "C29", pct = -10), "country", "CEE11")[
  , .(country_name, change_pct_of_gva = 100 * share_va)]

# ---- Q3. A 10% fall in US final demand: who loses most in CEE? -------------------------------
summarise_impact(fd_shock(yr, buyers = "US", pct = -10), "country", "CEE11")[
  , .(country_name, change_eur_m = dva, change_pct_of_gva = 100 * share_va)]

# ---- Q4. How much CEE GVA depends on government consumption, at home and abroad? --------------
demand_shares(c(2010, yr), "CEE11", "Government")[
  , .(year, country_name, home = round(100 * share_domestic, 1), foreign = round(100 * share_foreign, 2))]

# ---- Q5. German government consumption +EUR 10bn: what does CEE get? ---------------------------
summarise_impact(fd_shock(yr, buyers = "DE", categories = "Government", eur_m = 10000), "country", "CEE11")[
  order(-dva), .(country_name, gain_eur_m = round(dva, 1), gain_pct_of_gva = round(100 * share_va, 3))]

# ---- Q6. Most exposed pairs: CEE country-industry <- single foreign country-industry ------------
top_pairs(yr, "CEE11", type = "production", n_top = 20)[, .(origin, source, share = round(100 * share, 1))]

# ---- Q7. Over time: Polish GVA depending on German production --------------------------------
pl_on_de <- over_years(available_years(), function(y) {
  sources_of(y, "PL", type = "production", source_level = "country")[source == "DE", .(share)]
})
pl_on_de

# ---- Q8. Rough check against OECD TiVA: domestic VA in foreign final demand, Poland 2020 -------
# (TiVA FFD_DVA by partner; the shares should be of the same order: Germany about a quarter,
#  the US about 5-6% of Polish VA absorbed abroad)
pl20 <- va_by_buyer("PL", years = 2020)[domestic == FALSE, .(va = sum(va)), by = buyer_name]
pl20[, share_of_foreign := va / sum(va)][order(-va)][1:8]

# ---- Q9. Domestic shock: Polish government consumption +EUR 1bn ----------------------------------
# How much becomes Polish GVA, which Polish industries gain, and where the rest goes
pl_g <- domestic_shock(yr, "PL", categories = "Government", eur_m = 1000)
home_vs_abroad(pl_g, "PL")[, .(home_gva = round(dva_home), abroad = round(dva_abroad), home_per_eur = round(home_per_eur, 2))]
summarise_impact(pl_g, "sector", "PL")[order(-dva)][1:10, .(sector_name, gain_eur_m = round(dva, 1))]
summarise_impact(pl_g, "country")[country != "PL"][order(-dva)][1:8, .(country_name, gain_eur_m = round(dva, 1))]

# The same for a 10% fall in Polish construction output: which Polish suppliers lose most?
pl_f <- domestic_shock(yr, "PL", type = "production", sectors = "F", pct = -10)
summarise_impact(pl_f, "sector", "PL", min_va = min_va_eur_m)[in_source == FALSE][order(share_va)][1:10,
  .(sector_name, change_pct_of_own_gva = round(100 * share_va, 2))]

# ---- Q10. How much of each euro of home demand stays at home? CEE compared ------------------------
# Home GVA per EUR of final demand, by category (investment leaks most: machinery is imported)
cee_content <- rbindlist(lapply(country_groups$CEE11, function(cc) domestic_content(yr, cc)[, country := cc]))
dcast(cee_content[code != "P5M"], country ~ category, value.var = "home_per_eur")[
  , lapply(.SD, function(z) if (is.numeric(z)) round(z, 2) else z)]

# Polish industries whose final demand stays least at home (highest import content)
domestic_multipliers(yr, "PL")[va >= min_va_eur_m][order(va_home)][1:10,
  .(sector_name, home = round(va_home, 2), abroad = round(va_abroad, 2), output_multiplier = round(output_multiplier, 2))]

# ---- Q11. With household spending of extra wages (Type II): how much more? ------------------------
# Polish government consumption +EUR 1bn, at different spend rates (EUR of consumer spending per EUR of
# extra wages): home GVA per EUR of shock
rbindlist(lapply(c(0, 0.3, 0.45, 0.6), function(r) {
  h <- home_vs_abroad(domestic_shock(yr, "PL", categories = "Government", eur_m = 1000,
                                     induced = r > 0, spend_rate = r), "PL")
  data.table(spend_rate = r, home_gva_per_eur = round(h$home_per_eur, 3), abroad_per_eur = round(h$abroad_per_eur, 3))
}))

# German government +EUR 10bn: CEE gains, supply chain vs household spending (mostly German households)
summarise_impact(fd_shock(yr, buyers = "DE", categories = "Government", eur_m = 10000, induced = TRUE),
                 "country", "CEE11")[order(-dva), .(country_name, gain_eur_m = round(dva, 1),
                                                    of_which_household_spending = round(dva_induced, 1))]

# ---- Q12. Price shock: refined petroleum products +10% in every country --------------------------------
# Price change of Polish industries' output with full pass-through (price) and from their own fuel
# purchases alone (direct); extra cost as % of GVA = the squeeze on margins if they cannot pass it on
petro <- price_shock(yr, "World", "C19", pct = 10)
summarise_price(petro, "sector", "PL", min_va = min_va_eur_m)[1:10,
  .(sector_name, price_pct = round(100 * dp, 2), direct_pct = round(100 * dp_direct, 2),
    cost_eur_m = round(cost), cost_pct_of_gva = round(100 * cost_share_va, 1))]

# CEE household baskets (basic prices): a world price rise vs Polish refineries only
merge(basket_prices(petro, "CEE11", "Households")[, .(buyer_name, world = round(100 * dp, 2))],
      basket_prices(price_shock(yr, "PL", "C19", pct = 10), "CEE11", "Households")[
        , .(buyer_name, pl_refineries_only = round(100 * dp, 2))], by = "buyer_name")[order(-world)]
