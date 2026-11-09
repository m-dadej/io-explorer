## What this tool measures

All numbers come from the Eurostat **FIGARO** inter-country input-output tables (industry by industry): 49 countries plus the rest of the world, 64 industries each, 2010–2024, EUR million at current prices. Value added is **gross value added (GVA)**: compensation of employees + other net taxes on production + gross operating surplus and mixed income. Taxes on products are not GVA, so the sum of GVA is a little below GDP.

The model is the standard Leontief model:

- **A** = input coefficients (what each country-industry buys from each other one per euro of its output)
- **L** = (I − A)⁻¹, the Leontief inverse: output needed everywhere per euro of final demand
- **v** = GVA per euro of output

### Where value added ends up (tab 1)
GVA of country-industry *r* absorbed by final demand category *k* of buyer country *d* = v<sub>r</sub> · (L · y<sub>d,k</sub>)<sub>r</sub>. This counts every route: Polish steel in a German car bought by a US household counts for US households. Over all buyers and categories it adds up to 100% of the origin's GVA. **Direct exports vs value added** compares this with gross exports. Gross exports show where goods are shipped. Value added absorbed shows where the origin's own GVA is finally used.

### Exposure to a source, sources of an origin, top pairs (tabs 2–4)
Two ways a source can matter:

- **Production of a customer industry.** The source's output is set from outside the model. Its suppliers, at home and abroad, directly and through the supply chain, lose sales when it produces less. The share of GVA of *r* that depends on the production of *s* is D[r,s] = L[r,s]·x<sub>s</sub> / (L[s,s]·x<sub>r</sub>). For a group of sources S, the change in output is L<sub>RS</sub>·L<sub>SS</sub>⁻¹·Δx<sub>S</sub>. A 10% fall in the source's output removes 10% of this share. These shares are **not additive** across sources, because supply chains overlap: German car production and German machinery production partly need the same Polish steel.
- **Final demand for products.** The share of GVA of *r* that depends on final demand, by any buyer, for products of *s*: L[r,s]·y<sub>s</sub> / x<sub>r</sub>. These shares **are additive**. Over all products, home ones included, they sum to 100%.

The two give different answers for the same pair. Take German cars. "Production" covers all output of the German car industry, including the parts it sells to car makers abroad. "Final demand" covers only the cars bought by final users.

### Shock simulator (tab 5)
- **Final demand shock:** change any slice of final demand (buyer countries × categories × the country-industries making the products) by a % or by an amount. An amount is spread like current demand. ΔGVA = v · L · Δy.
- **Production shock:** change the output of some country-industries by a % or by an amount, as above. The shocked industries change by exactly the shock. Their suppliers change as described above.

**Parts of the effect.** The charts of the most affected industries split each change in GVA into the standard input-output parts. They add up to the total:

| Part | Final demand shock | Production shock |
|---|---|---|
| **Direct** | the industries whose products are bought: v·Δy | the shocked producers themselves: v·Δx<sub>S</sub> (left out of the charts) |
| **First-round suppliers** | what they buy directly as inputs: v·AΔy | v·A<sub>RS</sub>Δx<sub>S</sub> |
| **Further supply-chain rounds** | the suppliers' suppliers, and so on: v·(L − I − A)Δy | the rest of the supply-chain change |
| **Household spending** | when the option is on (see below) | when the option is on |

Example: Polish construction output +EUR 30m creates EUR 15.8m of Polish GVA:
- construction itself: 9.7;
- first-round suppliers: 3.7. Wholesale trade alone gets 0.55, from the merchants' margins on the materials construction buys;
- further rounds: 2.3.

The model is linear: no price effects and no substitution. Input structures are fixed at the chosen year. By default there are no income effects through household spending: it is an "open" model, so the effects are a lower bound for demand shocks. The option *Add household spending of extra wages* adds them (see below).

### Domestic shock (tab 6)
A shock inside one country, seen from that country: what happens to its own industries and how much leaks abroad.

- **Final demand:** the country's own buyers (households, government, investors, …) change their purchases of the chosen products. *Home-made and imported, as now* keeps their import shares, so part of the shock goes straight to foreign producers. *Home-made only* changes demand for home products only.
- **Production:** the output of home industries changes. Their suppliers at home and abroad follow, as in the simulator.
- **Home GVA per EUR 1 of shock** = change in home GVA / size of the shock.
  - The rest of each euro becomes GVA abroad: the leakage, through imported final goods and imported inputs, directly and further up the chain.
  - Or it pays taxes less subsidies on products bought as inputs.
  - In a production shock, the size is the output of the shocked industries, including what they buy from each other (e.g. construction subcontracting). So the ratio is lower than for a final demand shock to the same products.
- **Share of each home industry's GVA that depends on each kind of final demand:**
  - **Rows:** each home industry.
  - **Columns:** home final demand by category, plus all foreign final demand. Each row adds up to 100%.
  - **Reading it:** a 10% fall in a category of home demand (all products, import shares unchanged) lowers the industry's GVA by 10% of its share.
- **Where EUR 1 of home final demand ends up:**
  - **What is counted:** all products bought by the country's own buyers in each category, home-made and imported, at basic prices.
  - **The parts:** home GVA, GVA abroad, and taxes on products & other (taxes less subsidies on products paid on inputs, plus two small FIGARO adjustment rows). The three parts add up to 1.
  - **Link to the shock boxes:** this equals the home GVA per EUR of a shock to a whole category.
- **Where EUR 1 of final demand for a home industry's products ends up** (home multipliers), whoever buys the products:
  - the GVA of the industry itself;
  - the GVA of other home industries, i.e. its home supply chain;
  - GVA abroad;
  - taxes & other.
  
  The table adds the output multiplier: home output per EUR. The figures come from the global inverse, so they include the small feedback through foreign supply chains (e.g. Polish parts in German machines that Poland imports). Households as employers have no inputs, so all of their euro stays with them.

By default these are "Type I" effects: wages earned are not spent again. The next section describes the option that adds this spending.

### Household spending of extra wages (option in tabs 5 and 6)
Without the option, a shock reaches only the supply chain. With it, households also spend part of the extra wages the shock creates. That spending creates more output and wages, and so on, round after round. This is a "Type II" or induced effect, the Keynesian consumption multiplier inside the input-output model.

- **Extra wages:** compensation of employees (FIGARO row D1) per EUR of output, by country-industry.
- **Spending per EUR of extra wages** (default EUR 0.45, set in `config.R` and in the sidebar) combines two things:
  - the wedge from employer and employee contributions and income tax: for Poland roughly 0.55–0.6 of compensation reaches households;
  - the share of that net income that is spent: roughly 0.7–0.9.
- **What is bought:**
  - Households spend like they do now: the same basket and the same import shares.
  - Part of every EUR spent goes on VAT and other taxes on products, and on purchases abroad. For Poland, EUR 0.875 of each EUR reaches products at basic prices.
- **Every country's households** spend their extra wages, not only those of the shocked country. For a shock at home this makes almost no difference. For a shock abroad it matters: when German government spending creates German wages, German households buy CEE goods.
- **The loop is solved exactly:** extra wages w = W·Δx₁ + W·L·H·K·w, so w = (I − W·L·H·K)⁻¹·W·Δx₁, and extra output = L·H·K·w. Here W is wages per EUR of output by country, H the household spending mix of each country, and K the spending per EUR of wages at basic prices.

How much it adds, for Poland in 2024, home GVA per EUR of final demand:

| Final demand | Without | With household spending (0.45) |
|---|---|---|
| Government consumption | 0.85 | 1.00 |
| Household consumption | 0.63 | 0.69 |
| Investment | 0.57 | 0.64 |

It adds little because each round loses most of its value:
- only 46% of Polish GVA is compensation of employees;
- less than half of compensation is spent;
- 12.5% of each EUR spent goes on product taxes and purchases abroad;
- about a third of the rest is imported.

**What it assumes:**
- Only wages are spent. Profits and self-employed income are not passed on to households, which keeps the effect conservative.
- Extra output raises wages in proportion, and new spending follows the average basket.
- There are no prices, interest rates, crowding out, saving in anticipation of future taxes, capacity limits, or financing of the shock.

Read the result with household spending as the upper end of a pure short-run demand effect, and the result without it as the lower end. Neither replaces a macro estimate of fiscal multipliers.

In a production shock, household spending changes demand for all industries, the shocked ones included. In the charts that leave the shocked industries out, their part is left out too.

The structural view of which demand each home industry depends on splits the GVA that exists today, so it is always shown without household spending.

### Government consumption (tab 7)
Government final consumption (ESA code P3_S13) is mostly public services produced by the country itself (administration, education, health), plus goods and services provided to households (e.g. medicines). **Military equipment is investment (GFCF, P51G), not government consumption.** FIGARO does not show investment by the institution that buys it. For defence procurement scenarios, use the simulator with "Investment (GFCF)" and the defence-related industries (see config.R).

## Good to know

- **Small country-industries** can show extreme shares, even above 100% when their final demand is negative (stock drawdowns larger than their output). Rankings of shares leave out country-industries with GVA below the threshold in the sidebar (default from `config.R`).
- **Rest of world (FIGW1)** is one block for all countries not covered separately.
- **Current prices:** changes over time mix volumes, prices and exchange rates.
- **Data quality:** FIGARO is built from national supply-use tables and trade data and balanced by Eurostat. Detail for small countries and industries is partly estimated.

## Use the engine in R

The app calls functions in `R/engine.R`. You can call them yourself, e.g. in a Quarto report:

```r
root_dir <- "."; source("config.R"); source("R/engine.R")

# Where Polish manufacturing GVA ends up, by buyer and category
va_by_buyer("PL", "Manufacturing", years = 2024)

# German car production -10%: effect on CEE country-industries
summarise_impact(output_shock(2024, "DE", "C29", pct = -10), "country_sector", "CEE11")

# German government consumption +EUR 10bn: who gains?
summarise_impact(fd_shock(2024, buyers = "DE", categories = "Government", eur_m = 10000), "country")

# Polish government consumption +EUR 1bn: Polish industries hit, and the leakage abroad
g <- domestic_shock(2024, "PL", categories = "Government", eur_m = 1000)
home_vs_abroad(g, "PL")
summarise_impact(g, "sector", "PL")

# Home GVA per EUR of Polish final demand, by category, 2010-2024; home multipliers by industry
domestic_content(2010:2024, "PL")
domestic_multipliers(2024, "PL")

# The same shocks with households spending part of the extra wages (EUR 0.45 per EUR; or e.g. 0.6)
home_vs_abroad(domestic_shock(2024, "PL", categories = "Government", eur_m = 1000, induced = TRUE), "PL")
domestic_multipliers(2024, "PL", induced = TRUE, spend_rate = 0.6)
fd_shock(2024, buyers = "DE", categories = "Government", eur_m = 10000, induced = TRUE)
```

See `analysis/examples.R` for more, and the long tables in `data/tables/` (parquet) for your own analysis.
