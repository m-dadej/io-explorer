<!--
The guide box at the top of each tab, in the app and on the website. One section per tab:
  ## <tab id> | <the question the tab answers>
  A short text, always shown.
  ### How to read it
  Examples and hints, shown when the reader opens them.
Tab ids: dest, expo, src, pairs, sim, dom, gov.
After editing: restart the app; for the website run  root_dir <- "."; source("R/04_export_web_text.R")
-->

## dest | Who finally buys what a country or an industry produces?

Value added (GVA) is what producers earn from their sales after paying for the inputs they buy: mostly wages and profits. This tab follows the value added of the origin you pick to the final buyers who use it, wherever they live. It counts every route, including the route through other countries' exports.

### How to read it

- **A bar of 5% for Germany** means: of every EUR 100 of value added created by the origin, EUR 5 is finally used by German households, government or investors. Some of it is sold to Germany directly (Polish furniture bought in Germany). Some goes through other countries (Polish steel in a Czech car bought in Germany).
- **The boxes:** "at home" and "abroad" add up to 100%.
- **Direct exports vs value added:** a longer blue bar means the buyer gets more of the origin's value added than direct exports suggest, because it receives it inside other countries' products. A shorter blue bar means part of what is shipped there is sold on to other countries.
- **The heatmap** shows the same for each industry (or country) of the origin. Read along a row.
- **Try:** pick Manufacturing as the origin industry, or tick only "Investment (GFCF)" to see who buys through investment.

## expo | Who would lose if a big customer produced less, or if demand for some products fell?

Pick a source, for example car production in Germany. The chart ranks the countries and industries whose value added depends most on it, through all supply-chain links: sales to the source, sales to its suppliers, and so on.

### How to read it

- **A bar of 8% for "CZ Motor vehicles"** means: 8% of the value added of the Czech car industry depends on the source. If the source stopped completely, that much would be lost. If it fell by 10%, a tenth of it would be lost: 0.8% of the industry's value added.
- **Production or final demand?**
  - *Production* covers everything the source produces, including parts it sells to other factories. Use it for "what if this industry produces less" (a plant closes, a strike, a shortage of parts).
  - *Final demand* covers only what final users buy from the source: households, governments, investors. Use it for "what if people buy fewer of these products".
- **Shares are of each row's own value added.** They show how much the source matters to that row, not how big the row is. The Table tab has the amounts in EUR.
- **Try:** set "Results by" to Country for a summary by country.

## src | What does a country or an industry depend on most?

The mirror image of "Exposure to a source". Pick an origin, for example Poland, and see which industries or countries abroad its value added depends on most.

### How to read it

- **Production of customer industries:** a bar of 0.8% for "DE Motor vehicles" means that 0.8% of the origin's value added would be lost if German car production stopped. These shares overlap, so do not add them up: German car makers and German machinery makers partly use the same Polish steel.
- **Final demand for products:** a bar of 3.5% for Germany means that 3.5% of the origin's value added ends up in final products made in Germany, wherever they are bought (e.g. Polish parts in German cars sold in the US). These shares can be added. Over all sources, home included, they make 100%.
- **How this differs from "Where VA ends up":** that tab asks where the final buyer lives. This one asks whose products carry the origin's value added to the final buyer, or whose production it supplies.
- **Try:** choose Whole countries and Final demand for a short list by country.

## pairs | Which links between two industries are the tightest?

Searches all pairs of an origin country-industry and a source country-industry. It lists the pairs where the origin depends most on the source.

### How to read it

- **A row "CZ Motor vehicles ← DE Motor vehicles, 8%"** means: 8% of the value added of the Czech car industry depends on the German car industry. Read "←" as "depends on".
- **Very high shares** usually involve a small industry, or one that sells most of its output to one customer. Check the EUR columns in the table, and raise the GVA threshold to focus on larger industries.
- **Use it to find stories,** then look at them more closely in "Exposure to a source" and "Sources of an origin".

## sim | What happens if demand or production changes somewhere?

Set up a "what if", for example German households buy 10% less, or German car production falls 10%. Press "Run shock" to see how value added changes in each country and industry.

### How to read it

- **Results are changes in value added,** which is close to GDP. A result of −0.10% of a country's GVA means its value added in a year would be about 0.1% lower. As a rough guide, that takes 0.1 percentage points off GDP growth in the year the shock happens.
- **The parts of each bar:**
  - *Direct:* the producers of what is bought (or, in a production shock, the shocked producers).
  - *First-round suppliers:* what those producers buy as inputs.
  - *Further rounds:* the suppliers' suppliers, and so on.
  - *Household spending* (when switched on): workers spend part of the extra wages.
- **Effects are proportional:** −20% gives twice the effect of −10%, and +10% the same effect as −10% with the opposite sign.
- **What is left out:** prices, switching to other suppliers, capacity limits, policy responses and financial effects. Read the result as the exposure through trade and supply chains, not as a forecast.
- **Try:** in the Government tab, "Run in the shock simulator" sends a change in one government's consumption here.

## dom | If a country spends more at home, how much stays at home and how much leaks abroad?

A shock inside one country, for example the Polish government spends EUR 1 bn more. You see which of the country's own industries gain or lose, and how much goes to foreign producers through imports. The lower part of the page shows how the economy is built; it needs no shock.

### How to read it

- **"Home GVA per EUR 1 of shock" of EUR 0.85** means: each euro spent creates 85 cents of value added at home. The rest becomes value added abroad (imports, including the imported parts inside home-made products) or pays taxes on products.
- **Leakage abroad** is higher for spending with many imports (machinery, cars, fuel) and lower for services (public services, construction).
- **Household spending of extra wages:** results without it are a lower bound; results with it are the upper end of a pure demand effect. Neither includes price or interest-rate responses.
- **The heatmap** splits each industry's value added by who finally buys it. Read along a row: each row adds up to 100%. A 10% fall in one kind of demand lowers the industry's value added by a tenth of its share.
- **Where EUR 1 ends up:** a shorter blue part means more of the spending goes abroad.
- **Try:** tick Investment (GFCF) instead of Government and compare how much stays at home.

## gov | How much of an economy lives off government consumption?

Government consumption is spending on public services such as health, education and administration, and on goods provided to households. This tab shows how much of each country's value added it supports, from the country's own government and from foreign ones. It also shows who abroad benefits from one government's consumption.

### How to read it

- **A bar of 18% home and 2% foreign** means: 18% of the country's value added is finally used by its own government's consumption, and 2% by the consumption of other governments (e.g. medicines, IT services or parts that end up in foreign public services).
- **Not included:** government investment, such as roads or military equipment. It is part of "Investment (GFCF)"; use the shock simulator for it.
- **Who benefits:** rank by "% of own GVA" to see who depends most on that government, or by EUR million to see who gets the most.
- **"Run in the shock simulator"** sends a change in this government's consumption to the simulator, with all its results.
