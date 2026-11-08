# IO Explorer

Explore trade in value added between countries and industries, and test demand and production shocks. Data: Eurostat FIGARO inter-country input-output tables (industry by industry, 50 areas × 64 industries, 2010–2024, EUR million, current prices).

It answers questions such as:
- Where does Polish (Czech, CEE, …) value added end up? By buyer country and by type of final demand (households, government, investment, …), including indirect routes through other countries' exports.
- Which country-industries depend most on a given foreign industry? Both on its production (a supply-chain customer) and on final demand for its products.
- Which CEE country-industry is most exposed to which single foreign country-industry?
- What happens to value added everywhere if German car production falls 10%, or US final demand falls 5%, or the German government spends EUR 10bn more?
- How much of each country's value added depends on government consumption, at home and abroad?
- A shock at home: if Polish government consumption rises EUR 1bn, or Polish construction output falls 10%, which Polish industries gain or lose, and how much leaks abroad? How much of each euro of home demand stays at home, by category and by industry?

## How to run

1. Put the FIGARO flat files in `figaro_tables/`. File names look like `flatfile_eu-ic-io_ind-by-ind_26ed_2024.csv`; set the edition and years in `config.R`.
2. Open `io-explorer.Rproj` in RStudio and run `prepare_data.R`. This is needed once, and again for each new FIGARO edition. It takes about 15 minutes and 3 GB of disk.
3. Start the app: run `run_app.R`, or `shiny::runApp("app")`.

Every package it uses is common: data.table, arrow, shiny, bslib, reactable, ggplot2, writexl.

## The website (docs/)

`docs/` is a version of the app that runs entirely in the browser, for one year (the latest by default). It can be published for free with GitHub Pages: in the repository settings, choose Pages, branch `main`, folder `/docs`.

- **Tabs:** all the tabs of the R app, for one year. The "Over time" views are not on the website.
- **The engine:** `docs/js/engine.js` is a JavaScript copy of `R/engine.R`. Its results match the R engine to about 7 significant digits.
- **The data:** `R/03_export_web.R` writes it to `docs/data/<year>/`, about 85 MB:
  - the core tables (8 MB) load at once;
  - the Leontief inverse (41 MB) loads when a tab needs it;
  - the input coefficients (38 MB) load for shocks.

  Browsers keep the files in their cache, so later visits are fast.
- **Updating:** after a new FIGARO edition, run `prepare_data.R`, then `root_dir <- "."; source("R/03_export_web.R")`, and commit `docs/`.
- **Previewing on your PC:** run `httpuv::runStaticServer("docs", port = 8000)` in R and open http://localhost:8000. Opening `index.html` directly from disk does not work, because browsers don't let a page load data files from disk.
- **Libraries:** charts use Observable Plot, the pickers Tom Select, and Excel downloads SheetJS. All are copied into `docs/lib/`, so the site depends on no outside service.

## Files

| File | What it does |
|---|---|
| `config.R` | **All settings**: folders, FIGARO edition and years, country groups (CEE11, EU27, …), your own industry groups, household spend rate, ranking thresholds |
| `data/labels/countries.csv`, `sectors.csv` | Country names; industry names and broad groups |
| `prepare_data.R` | Runs the two data steps below |
| `R/01_import.R` | Reads each FIGARO CSV into matrices: intermediate use Z, final demand Y, value added. Checks that each table balances |
| `R/02_leontief.R` | Leontief inverse per year, GVA absorbed by each buyer country and final demand category. Checks that every country-industry's GVA is fully allocated. Writes long tables to `data/tables/` |
| `R/engine.R` | The functions that answer the questions (see the top of the file). Use them in your own scripts |
| `app/app.R` | The Shiny app; `app/charts.R` holds the chart style, `app/notes.md` the notes page |
| `analysis/examples.R` | Worked questions and checks that the model behaves (run it after a new edition) |
| `R/03_export_web.R` | Exports one year of the model for the website (`docs/data/`) and the notes page |
| `docs/` | The website: `index.html`, `js/engine.js` (the model), `js/main.js` (the tabs), `js/charts.js`, `js/ui.js`, `css/`, `lib/`, `data/` |

## The app

| Tab | What you see |
|---|---|
| Where VA ends up | An origin's GVA by buyer country and final demand category; direct exports vs value added; a heatmap by origin industry or country; over time |
| Exposure to a source | Who depends on a source (e.g. DE motor vehicles): share of GVA, by country-industry, country or industry; over time |
| Sources of an origin | What an origin (e.g. PL, or PL basic metals) depends on most: single foreign country-industries, whole countries or industries |
| Top pairs | The most exposed pairs of (origin country-industry, source country-industry) |
| Shock simulator | Any final demand or production shock, in % or EUR. Results by country, country-industry and industry group, split into direct, first-round suppliers, further supply-chain rounds and household spending; Excel download |
| Domestic shock | A shock to a country's own final demand or production. You see the effect on its own industries, the leakage abroad by country, and home GVA per EUR of shock. Three structural views need no shock: (1) which kind of home demand each industry depends on, (2) where EUR 1 of home demand ends up, by category and over time, and (3) home multipliers by industry |
| Government | GVA depending on government consumption at home and abroad, who benefits from a given government, a one-click scenario |
| Notes | Definitions, formulas, caveats |

Every chart has a Table tab with the numbers and an Excel download.

## Good to know

- **Two kinds of dependence.**
  - *Production of a customer industry*: the supply-chain link. The share of GVA lost if the source stopped producing; it scales linearly, so a 10% cut means 10% of it. These shares are not additive across sources.
  - *Final demand for products*: the share of GVA that depends on final buyers of the source's products. These shares add up to 100% over all products.
- **Government consumption excludes military equipment.** Equipment is investment (GFCF). For defence procurement scenarios use the simulator with "Investment (GFCF)" and the industry group "Defence-related manufacturing" from `config.R`.
- **The model is linear and open.** There are no price effects and no substitution, and input structures are those of the chosen year. By default there is no household-income feedback, so the effects of demand shocks are a lower bound.
- **Household spending of extra wages (option).** In the shock simulator and the Domestic shock tab, households can spend part of the extra wages a shock creates, round after round (a Type II, Keynesian consumption effect).
  - **The setting:** the spend rate per EUR of compensation of employees, default 0.45, set in `config.R` or in the sidebar.
  - **The effect:** Polish government consumption goes from EUR 0.85 to EUR 1.00 of home GVA per EUR.
  - **How to read it:** the result with this option is the upper end of a pure demand effect, and the result without it is the lower end. See the Notes tab.
- **Tiny country-industries** are left out of share rankings (GVA below EUR 100m by default; change it in `config.R` or in the app).
- **Zero or negative output.** A handful of country-industries (e.g. Albania's "households as employers" in 2017, −EUR 2,000) have zero or negative output; they are treated as producing nothing.
- **Long tables for your own work.** `data/tables/va_by_buyer.parquet` holds GVA by origin country-industry × buyer country × category × year. `direct_sales.parquet` holds gross sales by buyer country. `totals.parquet` holds output and GVA components. Open them with `arrow::read_parquet()`.
- **New FIGARO edition.** Change `edition` (and `years`) in `config.R`, put the files in `figaro_tables/`, delete `data/matrices` and `data/model`, run `prepare_data.R`, then `analysis/examples.R`.
