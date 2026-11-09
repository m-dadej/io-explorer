## What this tool measures

All numbers come from the Eurostat **FIGARO** inter-country input-output tables (industry by industry): 49 countries plus the rest of the world, 64 industries each, 2010–2024, EUR million at current prices. Value added is **gross value added (GVA)**: compensation of employees + other net taxes on production + gross operating surplus and mixed income. Taxes on products are not GVA, so the sum of GVA is a little below GDP.

The model is the standard Leontief model:

- **A** = input coefficients (what each country-industry buys from each other one per euro of its output)
- **L** = (I − A)⁻¹, the Leontief inverse: output needed everywhere per euro of final demand
- **v** = GVA per euro of output

### Where value added ends up
GVA of country-industry *r* absorbed by final demand category *k* of buyer country *d* = v<sub>r</sub> · (L · y<sub>d,k</sub>)<sub>r</sub>. This counts every route: Polish steel in a German car bought by a US household counts for US households. Over all buyers and categories it adds up to 100% of the origin's GVA. **Direct exports vs value added** compares this with gross exports. Gross exports show where goods are shipped. Value added absorbed shows where the origin's own GVA is finally used.

### Value added in exports
The other way round: not where an origin's GVA ends up, but what is inside its gross exports. For exports e<sub>r</sub> of country-industry *r* (to the chosen importers; sales within a country are not exports), the GVA of each source country-industry *i* in them is K[i,r] = v<sub>i</sub> · L[i,r] · e<sub>r</sub>. This is the OECD TiVA measure of value added in gross exports.

- **Home GVA:** sources in the exporting country. **Foreign GVA:** sources in other countries, in the inputs it imports, directly and further up the chain. **Taxes on products & other:** taxes less subsidies on products paid on inputs along the chain. The three add up to the exports.
- **Foreign GVA by source:** which countries or industries it comes from. With a group of exporters (e.g. CEE11), a country of the group is a foreign source for the others: Polish GVA in Czech exports is foreign GVA of Czechia.
- **Their GVA in other countries' exports:** the origin's GVA that leaves in the exports of other countries, v<sub>i</sub> · Σ<sub>j in d</sub> L[i,j] · e<sub>j</sub> for each exporting country *d*. Foreign GVA in exports looks at value chains from the buyer's side ("backward participation"); this looks at them from the supplier's side ("forward participation").
- **Two caveats.** Home GVA includes a little GVA that went abroad in exported inputs and came back home in imports. Value added in inputs that cross borders several times counts each time it is exported, so over all countries foreign GVA in exports is more than the value added traded.

Example, 2024: foreign GVA is 51% of Slovak and Hungarian exports, 43% of Czech and 30% of Polish exports. German GVA alone is 5.1% of Polish exports; Chinese GVA 2.5% (1.7% in 2010); Russian GVA fell from 4.8% in 2012 to 0.4%. Polish GVA in German exports is 2.1% of Polish GVA; in other countries' exports in all, 13.7%.

### Industry profile
One page about some country-industries: e.g. Polish motor vehicles, or motor vehicles in all CEE countries side by side. Totals over several country-industries weight each one by its output.

- **Key figures:** output, GVA and its share of the country's GVA; wage share; operating surplus; exports (sales to other countries) and imported inputs as % of output; home and foreign GVA per EUR of final demand for its products (whole supply chain, as the home multipliers in the Domestic shock tab); its GVA that ends up abroad or depends on government consumption; energy; upstreamness. With several countries there is one column per country.
- **Energy:** the industries in `energy_sectors` in `config.R` (by default mining and quarrying, refined petroleum products, electricity and gas; FIGARO's mining also has metal ores and stone), in all countries. *Bought directly* is what it buys from them per EUR of output. *Direct and through suppliers* is the price model with all energy prices doubled: the energy in all its inputs, along the whole chain, per EUR of output. *Energy prices +10%* is a tenth of that, as % of its GVA and of its gross operating surplus: the squeeze on margins if it cannot raise its own price.
- **Upstreamness:** the average number of production steps before its output reaches final users, (L·x)<sub>j</sub> / x<sub>j</sub>. 1 means it sells only to final users; the more of its output goes into other industries' inputs, the higher it is.
- **What each EUR of output pays for:** inputs from home industries, imported inputs, taxes on products paid on inputs, and its GVA: compensation of employees, gross operating surplus and mixed income, other net taxes on production. They add up to 100%.
- **Who buys the output:** single buyers (an industry in a country buying it as an input, or the households, government, investors … of a country) or buyer countries.
- **Where the inputs come from:** *bought directly* is A[i,j], what it buys from each supplier per EUR of output. *Value added along the whole supply chain* is v<sub>i</sub> · L[i,j], the GVA of each source per EUR of final demand for its products, its own GVA included.
- **Where its value added ends up:** as in Where VA ends up, with home first.

Example: Polish motor vehicles, 2024. Output EUR 52.9 bn, GVA EUR 9.9 bn (19% of output, 1.3% of Polish GVA). 70% of output is exported; imported inputs are 35% of output, and along the whole supply chain 47% of each euro of final demand for Polish cars is foreign GVA. 83% of its GVA ends up in final demand abroad. A 10% rise in energy prices everywhere costs it 5.7% of its GVA, 16% of its operating surplus.

### Exposure to a source, sources of an origin, top pairs
Two ways a source can matter:

- **Production of a customer industry.** The source's output is set from outside the model. Its suppliers, at home and abroad, directly and through the supply chain, lose sales when it produces less. The share of GVA of *r* that depends on the production of *s* is D[r,s] = L[r,s]·x<sub>s</sub> / (L[s,s]·x<sub>r</sub>). For a group of sources S, the change in output is L<sub>RS</sub>·L<sub>SS</sub>⁻¹·Δx<sub>S</sub>. A 10% fall in the source's output removes 10% of this share. These shares are **not additive** across sources, because supply chains overlap: German car production and German machinery production partly need the same Polish steel.
- **Final demand for products.** The share of GVA of *r* that depends on final demand, by any buyer, for products of *s*: L[r,s]·y<sub>s</sub> / x<sub>r</sub>. These shares **are additive**. Over all products, home ones included, they sum to 100%.

The two give different answers for the same pair. Take German cars. "Production" covers all output of the German car industry, including the parts it sells to car makers abroad. "Final demand" covers only the cars bought by final users.

### Shock simulator
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

The model is linear: these shocks have no price effects and no substitution (the Price shock tab is the price side of the same model, see below). Input structures are fixed at the chosen year. By default there are no income effects through household spending: it is an "open" model, so the effects are a lower bound for demand shocks. The option *Add household spending of extra wages* adds them (see below).

### Trade shock
A change in what some country-industries (the exporters) sell to some countries (the importers): a tariff, sanctions, a boycott. Unlike a final demand shock in the simulator, it hits **all exports**, inputs for the importers' industries as well as final products, or only one of the two. Sales within a country never count.

- **The shock:** the exporters' sales to the importers change by a % (or by an amount, spread like these exports). Each exporter's change in sales works like a change in final demand for its products: ΔGVA = v · L · Δs. The importers' own production does not change: their industries switch suppliers or do without the inputs.
- **Who makes up for the change:**
  - *No one:* the importers' buyers do without. The gross effect on exporters and their suppliers.
  - *Producers in the importing country:* the same industry there sells more, by the chosen share of the lost sales.
  - *Producers in other countries:* the same industry in all other countries (not the exporters' countries, not the importer) sells more, in proportion to what each sells to the importer now.
- **The split by channel**, for each country-industry:
  - *Own country's sales:* the change that comes from sales changes of producers in its own country: its own exports, and those of its home customers.
  - *Other countries' sales:* it supplies producers abroad whose sales change. For CEE, the indirect exposure through German exports.
  - With household spending switched on, the third part is spending of the extra (or lost) wages.
  
  The table also has the usual parts: direct (GVA on the change in sales itself), first-round suppliers, further rounds.

Example: EU27 exports to the US −10% (EUR 66 bn), no one makes up for them, 2024. CEE11 GVA falls EUR 4.7 bn, 0.23%; 30% of that comes through other countries' sales. Poland −0.25%, Estonia −0.33%, Hungary −0.28%. With only cars (−25%) and producers in other countries making up all of it, world GVA barely changes, but Slovakia loses 0.26% of its GVA and Mexico gains 0.14%.

What it assumes: the same linear model, no price effects. Lost sales are not redirected to other markets (unless producers elsewhere make up for them), and the importers' buyers do not change what they produce.

### Domestic shock
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

### Household spending of extra wages (option in the shock tabs)
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

### Price shock
The same tables, read the other way round. The price of each country-industry's output is the cost of its inputs plus its GVA per euro of output: p = p·A + v, with all prices equal to 1 in the table's year. The output price of the chosen producers S rises by a % set from outside the model, for instance by a world oil price. Every buyer passes its extra input costs on in full, along the whole supply chain, at home and abroad. The price of every other country-industry R moves by

dp<sub>R</sub> = dp<sub>S</sub> · L<sub>SS</sub>⁻¹ · L<sub>SR</sub> = dp<sub>S</sub> · A<sub>SR</sub> · (I − A<sub>RR</sub>)⁻¹

For a single producer j this is dp<sub>i</sub> = shock × L[j,i] / L[j,j]. The price shock uses row j of the Leontief inverse; the production shock uses column j. A price rise flows down the supply chain to the buyers, and a production cut flows up it to the suppliers.

- **Price change** of an industry's output = its extra input cost per EUR of output. It has two parts:
  - **Direct purchases:** the extra cost of what it buys from the producers themselves, shock × A[S,i]. This is the cost push if no one else passed anything on: the lower end.
  - **Through other suppliers:** what its other suppliers pass on. Together with the direct part this is full pass-through: the upper end.
- **Extra cost** = price change × output, in EUR at today's volumes. As % of GVA it is the squeeze on the industry's margins if it cannot raise its own price.
- **Totals** for a country, an industry or a group are output-weighted, i.e. the price index of what they produce. The producers whose price is set are left out of all totals and charts, since their price is the shock itself.
- **Baskets:** the price change of what a country's households, government or investors buy, home-made and imported, weighted by their purchases. The producers' own products bought directly (e.g. fuel bought by households) count with the full rise.

Example: refined petroleum products (C19) +10% in every country, Polish industries, 2024:
- land transport +1.70%, of which 1.39 pp from its own fuel; air transport +1.16%; chemicals +0.89%;
- travel agencies +0.84%, almost all of it through their suppliers (land and air transport, other travel agencies): an extra cost of 14% of their thin GVA;
- all Polish industries: an extra cost of EUR 6.7 bn, 0.9% of their GVA;
- the Polish household basket +0.64%, of which 0.33 pp is fuel bought directly. If only Polish refineries raise prices, imports keep their old price and the basket rises 0.46%.

**What it assumes:**
- Full pass-through and no substitution: no one absorbs the cost in margins or switches inputs, and volumes do not react. Read the total as the upper end of a short-run cost push, and the direct part as the lower end.
- GVA per unit stays as it is: wages and profits per unit do not react (no wage-price second round).
- Basic prices: before VAT, excise and other taxes on products. Excise is an amount per litre, so a given % rise at the pump is a larger % rise in basic prices. The household basket is therefore not the CPI: it leaves out taxes on products, and trade margins count as purchases from the trade industries.
- It is separate from the quantity shocks: here a higher price does not lower demand.
- For goods priced on world markets (oil products, gas, metals), choose World as the producers' countries. A rise in one country only leaves imported products at their old price.

### Government consumption
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

# Refined petroleum prices +10% everywhere: Polish industries' prices and costs, and the baskets
p <- price_shock(2024, "World", "C19", pct = 10)
summarise_price(p, "sector", "PL", min_va = 100)
basket_prices(p, "PL")

# EU exports to the US -10%, inputs and final products: CEE countries, split by channel
t <- trade_shock(2024, "EU27", importers = "US", pct = -10)
summarise_impact(t, "country", "CEE11")[, .(country, share_va, dva, dva_own, dva_other)]

# Value added in exports: home and foreign GVA in CEE exports; where the foreign GVA in Polish exports comes from
export_content(2024, "CEE11")
export_sources(2024, "PL", source_level = "country")
va_in_others_exports(2024, "PL")$by_exporter

# A profile of Polish motor vehicles (or of all CEE countries: profile_figures(2024, "CEE11", "C29"))
profile_figures(2024, "PL", "C29")
profile_costs(2024, "PL", "C29")
profile_buyers(2024, "PL", "C29")
profile_suppliers(2024, "PL", "C29", measure = "chain", level = "country")
```

See `analysis/examples.R` for more, and the long tables in `data/tables/` (parquet) for your own analysis.
