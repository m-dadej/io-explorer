# =============================================================================
# IO Explorer - Shiny app
# Start it from the project folder with shiny::runApp("app"), or open this file
# in RStudio and press "Run App". Needs the data from prepare_data.R.
# =============================================================================

root_dir <- normalizePath("..")   # Shiny runs the app from the app/ folder
source(file.path(root_dir, "config.R"), local = TRUE)
source(file.path(root_dir, "R/engine.R"), local = TRUE)
library(shiny)
library(bslib)
library(reactable)
library(ggplot2)
source("charts.R", local = TRUE)

yrs    <- available_years()
latest <- if (default_year %in% yrs) default_year else max(yrs)

# ---- Choices in the pickers ---------------------------------------------------------------
country_choices <- list(
  "Groups"    = setNames(names(country_groups), names(country_groups)),
  "Countries" = setNames(countries$code, paste0(countries$name, " (", countries$code, ")"))
)
sector_choices <- list(
  "All"                    = c("All sectors" = "All sectors"),
  "Broad groups"           = setNames(unique(sectors$group), unique(sectors$group)),
  "Your groups (config.R)" = setNames(names(custom_sector_groups), names(custom_sector_groups)),
  "Industries"             = setNames(sectors$code, paste0(sectors$code, "  ", sectors$name))
)
category_choices <- setNames(names(fd_categories), fd_categories)

# Short column names for the home demand heatmap
home_demand_short <- c(P3_S14 = "Households", P3_S13 = "Government", P3_S15 = "NPISH",
                       P51G = "Investment", P5M = "Inventories", FOREIGN = "Abroad")

# "Construction + Motor vehicles", or "all industries"
describe_sectors <- function(s) {
  if (identical(s, "All sectors")) return("all industries")
  paste(ifelse(s %in% names(sector_names), sector_names[s], s), collapse = " + ")
}

pick_countries <- function(id, label, selected) {
  selectizeInput(id, label, choices = country_choices, selected = selected, multiple = TRUE,
                 options = list(plugins = list("remove_button")))
}
pick_sectors <- function(id, label = "Industries", selected = "All sectors") {
  selectizeInput(id, label, choices = sector_choices, selected = selected, multiple = TRUE,
                 options = list(plugins = list("remove_button")))
}
pick_year <- function(id) selectInput(id, "Year", choices = rev(yrs), selected = latest)
pick_categories <- function(id, selected = names(fd_categories)) {
  checkboxGroupInput(id, "Final demand categories", choices = category_choices, selected = selected)
}
pick_min_va <- function(id) {
  numericInput(id, "Leave out country-industries with GVA below (EUR m)", min_va_eur_m, min = 0, step = 50)
}
pick_top <- function(id, value = top_n) numericInput(id, "Rows in the chart", value, min = 5, max = 60, step = 5)
# Household spending of extra wages: a switch and the spend rate (inputs <prefix>_induced, <prefix>_rate)
pick_induced <- function(prefix) {
  tagList(
    checkboxInput(paste0(prefix, "_induced"), "Add household spending of extra wages", FALSE),
    conditionalPanel(sprintf("input.%s_induced", prefix),
      numericInput(paste0(prefix, "_rate"), "Consumer spending per EUR of extra wages (EUR)",
                   household_spend_rate, min = 0, max = 1, step = 0.05))
  )
}
# The spend rate from an input, checked
spend_rate_or_stop <- function(x) {
  r <- num_or(x, household_spend_rate)
  validate(need(r >= 0 && r <= 1, "Consumer spending per EUR of extra wages must be between 0 and 1."))
  r
}
induced_text <- function(rate) sprintf("With household spending of extra wages (EUR %.2f per EUR).", rate)

# ---- Parts of a shock's effect (direct, first-round suppliers, further rounds, household spending) ----
eur_m1 <- function(x) paste0("EUR ", formatC(x, format = "f", digits = 1, big.mark = ","), " m")

# Long table for a stacked chart. `d` has columns direct, first, further, induced (EUR m); `scale`
# divides them (1 for EUR m, the GVA for shares). Parts that are zero in every row are left out.
effect_long <- function(labels, d, scale = 1) {
  parts <- c(direct = "Direct", first = "First-round suppliers", further = "Further supply-chain rounds",
             induced = "Household spending")
  long <- rbindlist(lapply(names(parts), function(k)
    data.table(label = labels, category = parts[[k]], value = d[[k]] / scale)))
  used <- long[, .(used = any(value != 0)), by = category][used == TRUE, category]
  long[category %in% used]
}
effect_chart <- function(labels, d, pct, x_title, caption = NULL) {
  long <- effect_long(labels, d, if (pct) d$va else 1)
  stacked_h(as.data.frame(long), x_title = x_title, caption = caption,
            colours = effect_colours[names(effect_colours) %in% long$category],
            percent = pct, expand = c(0.02, 0.06))
}

# One sentence with the parts of a total change in GVA
parts_text <- function(where, total, direct, first, further, induced, production, show_induced) {
  parts <- c(if (!production) paste("direct", eur_m1(direct))
             else if (direct != 0) paste("the shocked producers themselves", eur_m1(direct), "(not in the chart)"),
             paste("first-round suppliers", eur_m1(first)),
             paste("further supply-chain rounds", eur_m1(further)),
             if (show_induced) paste("household spending", eur_m1(induced)))
  paste0("Change in GVA ", where, ": ", eur_m1(total), " = ", paste(parts, collapse = ", "), ".")
}
hint <- function(...) div(class = "text-muted small", ...)

vbox <- function(title, output_id, note = NULL) {
  value_box(title = title, value = textOutput(output_id), if (!is.null(note)) p(note),
            theme = value_box_theme(bg = "#f6f5f1", fg = ink), fill = FALSE)
}

# A card with a chart, its table (with download) and optionally the same over time.
# `...` goes above the chart (e.g. a switch).
chart_card <- function(id, title, ..., time_panel = FALSE, note = NULL) {
  panels <- list(
    nav_panel("Chart", ..., plotOutput(paste0(id, "_plot"), height = "auto"),
              if (!is.null(note)) hint(note)),
    nav_panel("Table", reactableOutput(paste0(id, "_table")),
              div(class = "mt-2", downloadButton(paste0(id, "_dl"), "Download (Excel)",
                                                 class = "btn-sm btn-outline-secondary")))
  )
  if (time_panel) {
    panels <- c(panels, list(nav_panel("Over time", plotOutput(paste0(id, "_time"), height = "auto"),
                                       hint("Computed for all years when you open this tab."))))
  }
  do.call(navset_card_tab, c(list(id = paste0(id, "_tabs"), title = title, full_screen = TRUE), panels))
}

# ---- Tables -----------------------------------------------------------------------------------
# `cols`: named list, column -> c(header, type); type is "text", "pct" (0-1), "eur" (EUR m), "eur1",
# "dec" (2 decimals), "num"
table_view <- function(df, cols, page = 15) {
  df <- as.data.frame(df)[, names(cols), drop = FALSE]
  defs <- lapply(cols, function(spec) {
    switch(spec[2],
      pct  = colDef(name = spec[1], format = colFormat(percent = TRUE, digits = 2), align = "right"),
      eur  = colDef(name = spec[1], format = colFormat(separators = TRUE, digits = 0), align = "right"),
      eur1 = colDef(name = spec[1], format = colFormat(separators = TRUE, digits = 1), align = "right"),
      dec  = colDef(name = spec[1], format = colFormat(digits = 2), align = "right"),
      num  = colDef(name = spec[1], align = "right"),
      colDef(name = spec[1], minWidth = 170)
    )
  })
  reactable(df, columns = defs, searchable = TRUE, compact = TRUE, striped = TRUE, highlight = TRUE,
            defaultPageSize = page, showPageSizeOptions = TRUE, pageSizeOptions = c(15, 50, 200))
}
export_view <- function(df, cols) {
  df <- as.data.frame(df)[, names(cols), drop = FALSE]
  names(df) <- vapply(cols, `[`, "", 1)
  df
}

# Chart + table + download for one card
# `cols` can be a list or a function returning one (when the columns depend on inputs).
wire_card <- function(output, id, data, cols, plot, height, file_stub) {
  get_cols <- function() if (is.function(cols)) cols() else cols
  output[[paste0(id, "_plot")]]  <- renderPlot(plot(), height = height, res = 96)
  output[[paste0(id, "_table")]] <- renderReactable(table_view(data(), get_cols()))
  output[[paste0(id, "_dl")]]    <- downloadHandler(
    filename = function() paste0(file_stub, "_", Sys.Date(), ".xlsx"),
    content  = function(file) writexl::write_xlsx(export_view(data(), get_cols()), file)
  )
}

# A numeric input, or a default when the box is empty
num_or <- function(x, default) if (is.null(x) || length(x) == 0 || is.na(x)) default else x

# Value added that depends on a source: its production (-100% shock, sign turned) or the final
# demand for its products
dependence_impact <- function(year, type, src_c, src_s) {
  if (type == "production") {
    imp <- output_shock(year, src_c, src_s, pct = -100, decompose = FALSE)
    imp[, dva := -dva]
  } else {
    imp <- va_from_final_demand(year, product_countries = src_c, product_sectors = src_s)
  }
  imp
}
dependence_table <- function(imp, type, level, show_c, min_va) {
  s <- summarise_impact(imp, level, show_c, min_va)
  if (type == "production") {
    s <- s[in_source == FALSE]          # the source itself is not "dependent"
    s[, dep := dva_excl_source]
  } else {
    s[, dep := dva]
  }
  s[, share := dep / va]
  s[, name := switch(level, country = country_name, country_sector = label,
                     sector = sector_name, sector_group = sector_group)]
  s[order(-share)]
}
name_col <- function(level) {
  switch(level, country = "Country", country_sector = "Country-industry", sector = "Industry",
         sector_group = "Industry group")
}

# ==== UI =============================================================================================
ui <- page_navbar(
  id = "tabs",
  title = "IO Explorer",
  theme = bs_theme(version = 5, primary = blue, "font-size-base" = "0.95rem"),
  fillable = FALSE,

  # ---- 1. Where value added ends up -------------------------------------------------------------
  nav_panel("Where VA ends up", value = "dest",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(width = 310,
        pick_countries("d_c", "Origin: countries", default_origin),
        pick_sectors("d_s", "Origin: industries"),
        pick_year("d_year"),
        pick_categories("d_cats"),
        pick_top("d_top", 15),
        hint("Value added (GVA) of the origin that ends up in final demand of each buyer country, ",
             "directly or through other countries' exports (e.g. Polish parts in German cars bought in the US ",
             "count for the US). Shares are of the origin's GVA.")
      ),
      layout_column_wrap(width = 1 / 4, fill = FALSE,
        vbox("Gross value added", "d_gva"),
        vbox("Ends up at home", "d_home", "share of GVA, all categories"),
        vbox("Ends up abroad", "d_abroad", "share of GVA, all categories"),
        vbox("Depends on government consumption", "d_gov", "at home and abroad")
      ),
      chart_card("d_top", "Foreign buyers: share of the origin's GVA absorbed by their final demand",
                 time_panel = TRUE),
      chart_card("d_exp", "Direct exports vs value added absorbed (shares of all foreign buyers)",
                 note = paste("Gross exports count where goods and services are shipped to. Value added",
                              "absorbed counts where the origin's own value added is finally used. A buyer",
                              "with a longer blue bar takes more of the origin's value added than direct",
                              "exports suggest (it is reached through other countries).")),
      chart_card("d_heat", "Share of GVA absorbed, by part of the origin (%)",
                 radioButtons("d_heat_rows", NULL, c("By industry" = "sector", "By country" = "country"),
                              inline = TRUE),
                 note = paste("Home = final demand in the row's own country (not coloured, so that differences",
                              "between foreign buyers show). Largest 15 rows by GVA, top 8 foreign buyers."))
    )
  ),

  # ---- 2. Exposure to a source ----------------------------------------------------------------------
  nav_panel("Exposure to a source", value = "expo",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(width = 310,
        radioButtons("x_type", "The source is",
                     c("Production of a customer industry" = "production",
                       "Final demand for products" = "final_demand")),
        pick_countries("x_src_c", "Source: countries", "DE"),
        pick_sectors("x_src_s", "Source: industries", "C29"),
        pick_year("x_year"),
        hr(),
        pick_countries("x_show", "Show results for", default_focus),
        radioButtons("x_level", "Results by",
                     c("Country-industry" = "country_sector", "Country" = "country",
                       "Industry" = "sector", "Broad industry group" = "sector_group")),
        pick_min_va("x_min_va"),
        pick_top("x_top"),
        hint(strong("Production: "), "share of GVA that would disappear if the source stopped producing ",
             "(its purchases of inputs, direct and through the supply chain). A 10% fall in the source's ",
             "output takes away 10% of this.", br(),
             strong("Final demand: "), "share of GVA that depends on final demand, anywhere, for the ",
             "source's products.")
      ),
      layout_column_wrap(width = 1 / 3, fill = FALSE,
        vbox(textOutput("x_size_title", inline = TRUE), "x_size"),
        vbox("GVA depending on it in the countries shown", "x_dep"),
        vbox("Share of their GVA", "x_share")
      ),
      chart_card("x_res", textOutput("x_title", inline = TRUE), time_panel = TRUE)
    )
  ),

  # ---- 3. What an origin depends on ---------------------------------------------------------------------
  nav_panel("Sources of an origin", value = "src",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(width = 310,
        pick_countries("o_c", "Origin: countries", default_origin),
        pick_sectors("o_s", "Origin: industries"),
        radioButtons("o_type", "Depends on",
                     c("Production of customer industries" = "production",
                       "Final demand for products" = "final_demand")),
        radioButtons("o_level", "Sources",
                     c("Single country-industries" = "country_sector",
                       "Whole countries" = "country",
                       "Industries, all countries together (final demand only)" = "sector")),
        pick_countries("o_src_c", "Look for sources in", "World"),
        checkboxInput("o_foreign", "Only sources outside the origin's countries", TRUE),
        pick_year("o_year"),
        pick_top("o_top"),
        hint("Production: the shares are not additive (supply chains overlap). ",
             "Final demand: the shares add up to 100% over all sources, home included.")
      ),
      chart_card("o_res", textOutput("o_title", inline = TRUE))
    )
  ),

  # ---- 4. Most exposed pairs --------------------------------------------------------------------------------
  nav_panel("Top pairs", value = "pairs",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(width = 310,
        pick_countries("p_oc", "Origins: countries", default_focus),
        pick_sectors("p_os", "Origins: industries"),
        pick_countries("p_sc", "Sources: countries", "World"),
        pick_sectors("p_ss", "Sources: industries"),
        radioButtons("p_type", "Dependence on",
                     c("Production of the source" = "production",
                       "Final demand for the source's products" = "final_demand")),
        checkboxInput("p_foreign", "Only sources in another country", TRUE),
        pick_min_va("p_min_va"),
        numericInput("p_n", "Pairs in the table", 100, min = 10, max = 1000, step = 10),
        pick_year("p_year"),
        hint("Each pair: one origin country-industry and one source country-industry. ",
             "Share = part of the origin's GVA that depends on the source.")
      ),
      chart_card("p_res", "Most exposed pairs of country-industries")
    )
  ),

  # ---- 5. Shock simulator ---------------------------------------------------------------------------------------
  nav_panel("Shock simulator", value = "sim",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(width = 310,
        radioButtons("s_type", "Type of shock",
                     c("Final demand" = "final_demand", "Production" = "production"), inline = TRUE),
        conditionalPanel("input.s_type == 'final_demand'",
          pick_countries("s_buyers", "Buyers: countries", "DE"),
          pick_categories("s_cats"),
          pick_countries("s_pc", "Products made in", "World"),
          pick_sectors("s_ps", "Products of industries")
        ),
        conditionalPanel("input.s_type == 'production'",
          pick_countries("s_src_c", "Producers: countries", "DE"),
          pick_sectors("s_src_s", "Producers: industries", "C29")
        ),
        radioButtons("s_unit", "Size given as", c("% change" = "pct", "EUR million" = "eur"), inline = TRUE),
        numericInput("s_size", "Size", -10),
        pick_year("s_year"),
        pick_induced("s"),
        actionButton("s_run", "Run shock", class = "btn-primary w-100 mb-2"),
        hr(),
        pick_countries("s_show", "Show results for", default_focus),
        pick_min_va("s_min_va"),
        pick_top("s_top"),
        hint("An amount in EUR is spread like current demand (or output). Effects are linear: ",
             "twice the shock gives twice the effect. Household spending of extra wages: households in ",
             "every country spend part of the extra wages, round after round (see Notes).")
      ),
      div(class = "mb-2", strong(textOutput("s_desc"))),
      layout_column_wrap(width = 1 / 3, fill = FALSE,
        vbox("Size of the shock", "s_size_out"),
        vbox("Change in world GVA", "s_world", textOutput("s_world_note", inline = TRUE)),
        vbox("Change in GVA of the countries shown", "s_shown", textOutput("s_shown_note", inline = TRUE))
      ),
      chart_card("s_country", "Change in GVA by country (% of the country's GVA)",
                 div(class = "d-flex flex-wrap column-gap-4",
                     radioButtons("s_country_scope", NULL,
                                  c("Countries shown" = "shown", "20 most affected worldwide" = "world"),
                                  inline = TRUE),
                     conditionalPanel("input.s_country_scope == 'world'",
                       checkboxInput("s_excl_shocked", "Leave out the countries where the shock happens", TRUE)))),
      chart_card("s_cs", "Most affected country-industries (% of their own GVA)",
                 div(class = "small text-muted mb-1", textOutput("s_cs_parts")),
                 note = paste("Direct = the producers of what is bought. First-round suppliers = what they buy as",
                              "inputs. Further rounds = the suppliers' suppliers, and so on. Household spending =",
                              "spending of the extra wages (when switched on). In a production shock, the shocked",
                              "producers themselves are left out of the chart.")),
      chart_card("s_heat", "Change in GVA by broad industry group (% of the group's GVA)"),
      downloadButton("s_dl_all", "Download all results (Excel)", class = "btn-outline-secondary mb-4")
    )
  ),

  # ---- 6. Domestic shock: a country's own demand or production, and its own industries ---------------------------
  nav_panel("Domestic shock", value = "dom",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(width = 310,
        selectInput("h_c", "Country", choices = setNames(countries$code, countries$name), selected = default_origin),
        pick_year("h_year"),
        hr(),
        radioButtons("h_type", "Shock to the country's own",
                     c("Final demand" = "final_demand", "Production" = "production"), inline = TRUE),
        conditionalPanel("input.h_type == 'final_demand'",
          pick_categories("h_cats", selected = "P3_S13"),
          pick_sectors("h_fd_s", "Products of industries"),
          radioButtons("h_from", "Products bought",
                       c("Home-made and imported, as now" = "all", "Home-made only" = "home"))
        ),
        conditionalPanel("input.h_type == 'production'",
          pick_sectors("h_prod_s", "Home industries producing", "F")
        ),
        radioButtons("h_unit", "Size given as", c("EUR million" = "eur", "% change" = "pct"), inline = TRUE),
        numericInput("h_size", "Size", 1000),
        pick_induced("h"),
        hr(),
        radioButtons("h_level", "Results by", c("Industry" = "sector", "Broad industry group" = "sector_group"),
                     inline = TRUE),
        pick_min_va("h_min_va"),
        pick_top("h_top"),
        hint(strong("Final demand: "), "the country's own households, government, investors, ... change their ",
             "purchases. \"As now\" keeps their import shares, so part of the shock goes straight abroad.", br(),
             strong("Production: "), "the output of home industries changes; their suppliers at home and ",
             "abroad follow.", br(),
             strong("Household spending: "), "households spend part of the extra wages, round after round ",
             "(at home and on imports). Without it, effects are a lower bound; with it, the upper end of a ",
             "pure demand effect (no prices, interest rates or crowding out).")
      ),
      h5(class = "mt-1", "The shock"),
      div(class = "mb-2", textOutput("h_desc")),
      layout_column_wrap(width = 1 / 4, fill = FALSE,
        vbox("Size of the shock", "h_size_out", textOutput("h_size_note", inline = TRUE)),
        vbox("Change in home GVA", "h_home", textOutput("h_home_note", inline = TRUE)),
        vbox("Home GVA per EUR 1 of shock", "h_mult_out", textOutput("h_mult_note", inline = TRUE)),
        vbox("Change in GVA abroad", "h_abroad_out", textOutput("h_abroad_note", inline = TRUE))
      ),
      chart_card("h_ind", "Change in GVA of home industries",
                 radioButtons("h_metric", NULL, c("EUR million" = "eur", "% of own GVA" = "share"), inline = TRUE),
                 div(class = "small text-muted mb-1", textOutput("h_ind_parts")),
                 note = paste("Direct = the industries whose products are bought. First-round suppliers = what",
                              "they buy as inputs. Further rounds = the suppliers' suppliers, and so on.",
                              "Household spending = spending of the extra wages (when switched on).",
                              "In a production shock, the shocked industries themselves are left out (they change",
                              "by exactly the shock); by industry group, only the change outside them is counted.")),
      chart_card("h_abroad", "Change in GVA abroad, by country (leakage through imports)"),
      h5(class = "mt-4", "How the home economy is built"),
      hint(class = "mb-2", "These views need no shock: they show the country's structure in the chosen year."),
      chart_card("h_expo", "Share of each home industry's GVA that depends on each kind of final demand (%)",
                 note = paste("Home final demand by category, and all foreign final demand. Each row adds up to",
                              "100%. A 10% fall in a category of home demand lowers the industry's GVA by",
                              "10% of its share (before household spending of the lost wages).")),
      chart_card("h_content", "Where EUR 1 of home final demand ends up", time_panel = TRUE,
                 note = paste("All products bought by the country's own buyers, home-made and imported, at basic",
                              "prices. Taxes on products & other = taxes less subsidies on products paid on",
                              "inputs along the chain. Inventories are in the table only (small, can be negative).",
                              "With household spending on, the green part is the extra home GVA it brings, on",
                              "top of the EUR 1.")),
      chart_card("h_mult", "Where EUR 1 of final demand for a home industry's products ends up",
                 radioButtons("h_mult_order", NULL,
                              c("Most stays at home" = "high", "Least stays at home" = "low"), inline = TRUE),
                 note = paste("Per EUR 1 of final demand for the industry's products, whoever buys them.",
                              "Other home industries = its home supply chain. The table also has the output",
                              "multiplier (home output per EUR). With household spending on, the green part is",
                              "the extra home GVA it brings, on top of the EUR 1."))
    )
  ),

  # ---- 7. Government consumption ---------------------------------------------------------------------------------
  nav_panel("Government", value = "gov",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(width = 310,
        pick_year("g_year"),
        pick_countries("g_show", "Countries", default_focus),
        hr(),
        selectInput("g_gov", "Government of", choices = setNames(countries$code, countries$name),
                    selected = "DE"),
        radioButtons("g_level", "Who benefits, by",
                     c("Country-industry" = "country_sector", "Country" = "country", "Industry" = "sector")),
        radioButtons("g_metric", "Rank by", c("% of own GVA" = "share", "EUR million" = "eur"), inline = TRUE),
        pick_min_va("g_min_va"),
        pick_top("g_top"),
        hr(),
        numericInput("g_amount", "Scenario: change in this government's consumption (EUR m)", 10000, step = 1000),
        actionButton("g_sim", "Run in the shock simulator", class = "btn-outline-primary w-100"),
        hint(class = "mt-2", "Government consumption (P3_S13) covers public services (health, education, ",
             "administration, defence services) and goods provided to households. Military equipment is ",
             "investment (GFCF), not consumption: for defence procurement use the simulator with ",
             "\"Investment (GFCF)\" and the defence-related industries.")
      ),
      chart_card("g_share", "Share of GVA that depends on government consumption",
                 note = "Home = the country's own government. Foreign = all other governments."),
      chart_card("g_time", "Over time",
                 radioButtons("g_time_measure", NULL,
                              c("Foreign governments" = "foreign", "Home government" = "home", "Both" = "both"),
                              inline = TRUE)),
      chart_card("g_benef", textOutput("g_benef_title", inline = TRUE))
    )
  ),

  # ---- 8. Notes ----------------------------------------------------------------------------------------------------
  nav_panel("Notes", value = "notes",
    div(class = "container", style = "max-width: 860px", markdown(paste(readLines(file.path(root_dir, "app/notes.md"), encoding = "UTF-8"), collapse = "
")))
  ),

  nav_spacer(),
  nav_item(span(class = "text-muted small", "Eurostat FIGARO ", edition, ", EUR m, current prices"))
)

# ==== Server =========================================================================================
server <- function(input, output, session) {

  need_sel <- function(...) validate(need(all(vapply(list(...), length, 1L) > 0), "Make a choice on the left."))

  # ---- 1. Where value added ends up -----------------------------------------------------------------------
  d_sel <- reactive({
    need_sel(input$d_c, input$d_s)
    validate(need(length(input$d_cats) > 0, "Choose at least one final demand category."))
    list(c = input$d_c, s = input$d_s, year = as.integer(input$d_year),
         cats = unname(fd_categories[input$d_cats]), top = max(1, num_or(input$d_top, 15)))
  })
  d_all <- reactive({ s <- d_sel(); va_by_buyer(s$c, s$s, s$year) })
  d_origin_va <- reactive(d_all()$origin_va[1])

  output$d_gva    <- renderText(fmt_bn(d_origin_va()))
  output$d_home   <- renderText(fmt_pct(d_all()[domestic == TRUE, sum(va)] / d_origin_va()))
  output$d_abroad <- renderText(fmt_pct(d_all()[domestic == FALSE, sum(va)] / d_origin_va()))
  output$d_gov    <- renderText(fmt_pct(d_all()[category == "Government", sum(va)] / d_origin_va()))

  d_buyers <- reactive({
    s <- d_sel()
    b <- d_all()[category %in% s$cats]
    wide <- dcast(b, buyer + buyer_name + domestic ~ category, value.var = "share", fun.aggregate = sum)
    wide[, total := rowSums(.SD), .SDcols = s$cats]
    wide[b[, .(va = sum(va)), by = buyer], va := i.va, on = "buyer"]
    wide[domestic == TRUE, buyer_name := paste(buyer_name, "(home)")]
    wide[order(-domestic, -total)]
  })
  d_foreign_top <- reactive({
    w <- d_buyers()[domestic == FALSE]
    head(w$buyer_name, d_sel()$top)
  })

  wire_card(output, "d_top", d_buyers,
    cols = function() c(list(buyer_name = c("Buyer", "text")),
                        setNames(lapply(d_sel()$cats, function(k) c(k, "pct")), d_sel()$cats),
                        list(total = c("Total share", "pct"), va = c("GVA absorbed, EUR m", "eur"))),
    plot = function() {
      s <- d_sel()
      d <- d_all()[domestic == FALSE & category %in% s$cats & buyer_name %in% d_foreign_top(),
                   .(label = buyer_name, category, value = share)]
      d$label <- factor(d$label, levels = d_foreign_top())
      d <- d[order(label)]
      home <- d_buyers()[domestic == TRUE, sum(total)]
      stacked_h(as.data.frame(d), x_title = "Share of the origin's GVA",
                caption = paste0(describe(s$c, s$s), ", ", s$year, ". Not shown: home final demand, ",
                                 fmt_pct(home), " of GVA."))
    },
    height = function() bar_height(d_sel()$top),
    file_stub = "va_by_buyer"
  )

  output$d_top_time <- renderPlot({
    req(input$d_top_tabs == "Over time")
    s <- d_sel()
    d <- va_by_buyer(s$c, s$s, yrs)[domestic == FALSE & category %in% s$cats,
                                    .(value = sum(share)), by = .(year, series = buyer_name)]
    lines_t(d, levels = head(d_foreign_top(), 6), y_title = "Share of the origin's GVA",
            caption = paste0(describe(s$c, s$s), ". Six largest foreign buyers in ", s$year, "."))
  }, height = 440, res = 96)

  d_exp <- reactive({ s <- d_sel(); exports_vs_va(s$c, s$s, s$year) })
  wire_card(output, "d_exp", d_exp,
    cols = list(buyer_name = c("Buyer", "text"), gross_exports = c("Gross exports, EUR m", "eur"),
                exports_share = c("Share of gross exports", "pct"), va_absorbed = c("GVA absorbed, EUR m", "eur"),
                va_share = c("Share of GVA absorbed abroad", "pct")),
    plot = function() {
      s <- d_sel()
      top <- head(d_exp(), s$top)
      d <- rbind(data.frame(label = top$buyer_name, measure = "Gross exports", value = top$exports_share),
                 data.frame(label = top$buyer_name, measure = "Value added absorbed", value = top$va_share))
      paired_h(d, c("Gross exports", "Value added absorbed"), x_title = "Share of all foreign buyers",
               caption = paste0(describe(s$c, s$s), ", ", s$year, ". All final demand categories."))
    },
    height = function() bar_height(d_sel()$top, per_bar = 34),
    file_stub = "exports_vs_va"
  )

  d_heat <- reactive({
    s <- d_sel()
    lvl <- input$d_heat_rows
    b <- va_by_buyer(s$c, s$s, s$year, origin_level = lvl)[category %in% s$cats]
    b[, col := ifelse(domestic, "Home", short_code(buyer))]   # country codes keep the columns narrow
    rows <- unique(b[, .(origin, origin_va)])[order(-origin_va)][1:min(.N, 15), origin]
    cols <- c("Home", short_code(head(d_buyers()[domestic == FALSE]$buyer, 8)))
    out <- b[origin %in% rows & col %in% cols, .(value = sum(share)), by = .(origin, col)]
    out[, row := if (lvl == "sector") sector_names[origin] else country_names[origin]]
    list(long = out, rows = unname(if (lvl == "sector") sector_names[rows] else country_names[rows]), cols = cols)
  })
  d_heat_wide <- reactive({
    h <- d_heat()
    w <- dcast(h$long, row ~ col, value.var = "value", fill = 0)
    w <- w[match(h$rows, row), c("row", h$cols), with = FALSE]
    w
  })
  wire_card(output, "d_heat", d_heat_wide,
    cols = function() c(list(row = c("Origin", "text")),
                        setNames(lapply(d_heat()$cols, function(k) c(k, "pct")), d_heat()$cols)),
    plot = function() {
      h <- d_heat()
      heatmap_t(as.data.frame(h$long), h$rows, h$cols, plain_cols = "Home",
                caption = paste0(describe(d_sel()$c, d_sel()$s), ", ", d_sel()$year,
                                 ". Values in % of the row's GVA. RoW = rest of world."))
    },
    height = function() 120 + 30 * length(d_heat()$rows),
    file_stub = "va_by_buyer_heatmap"
  )

  # ---- 2. Exposure to a source ----------------------------------------------------------------------------------
  x_impact <- reactive({
    need_sel(input$x_src_c, input$x_src_s, input$x_show)
    tryCatch(dependence_impact(as.integer(input$x_year), input$x_type, input$x_src_c, input$x_src_s),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  x_res <- reactive(dependence_table(x_impact(), input$x_type, input$x_level, input$x_show, num_or(input$x_min_va, 0)))
  x_total <- reactive({
    t <- summarise_impact(x_impact(), "total", input$x_show)
    dep <- if (input$x_type == "production") t$dva_excl_source else t$dva
    list(dep = dep, share = dep / t$va)
  })

  output$x_size_title <- renderText(if (input$x_type == "production") "Output of the source" else "Final demand for the source's products")
  output$x_size  <- renderText(fmt_bn(abs(attr(x_impact(), "shock")$size)))
  output$x_dep   <- renderText(fmt_bn(x_total()$dep))
  output$x_share <- renderText(fmt_pct(x_total()$share, 2))
  output$x_title <- renderText(paste0(
    "Share of GVA that depends on ",
    if (input$x_type == "production") "the production of " else "final demand for products of ",
    describe(input$x_src_c, input$x_src_s)))

  wire_card(output, "x_res", x_res,
    cols = function() list(name = c(name_col(input$x_level), "text"), share = c("Share of GVA dependent", "pct"),
                           dep = c("GVA dependent, EUR m", "eur1"), va = c("GVA, EUR m", "eur")),
    plot = function() {
      d <- head(x_res(), num_or(input$x_top, top_n))
      bars_h(d$name, d$share, x_title = "Share of GVA",
             caption = paste0(input$x_year, ". Results for: ", describe(input$x_show), "."))
    },
    height = function() bar_height(min(nrow(x_res()), num_or(input$x_top, top_n))),
    file_stub = "exposure_to_source"
  )

  output$x_res_time <- renderPlot({
    req(input$x_res_tabs == "Over time")
    top <- head(x_res()$name, 6)
    d <- withProgress(message = "Computing all years", value = 0, {
      rbindlist(lapply(yrs, function(y) {
        incProgress(1 / length(yrs))
        r <- dependence_table(dependence_impact(y, input$x_type, input$x_src_c, input$x_src_s),
                              input$x_type, input$x_level, input$x_show, num_or(input$x_min_va, 0))
        r[name %in% top, .(year = y, series = name, value = share)]
      }))
    })
    lines_t(d, levels = top, y_title = "Share of GVA",
            caption = paste("Six most exposed in", input$x_year, "."))
  }, height = 440, res = 96)

  # ---- 3. Sources of an origin ------------------------------------------------------------------------------------
  o_res <- reactive({
    need_sel(input$o_c, input$o_s, input$o_src_c)
    validate(need(!(input$o_type == "production" && input$o_level == "sector"),
                  "For production, choose single country-industries or whole countries as sources."))
    tryCatch(sources_of(as.integer(input$o_year), input$o_c, input$o_s, type = input$o_type,
                        source_level = input$o_level, source_countries = input$o_src_c,
                        foreign_only = input$o_foreign),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$o_title <- renderText(paste0(
    "What ", describe(input$o_c, input$o_s), " depends on: ",
    if (input$o_type == "production") "production of its customers" else "final demand for products"))

  wire_card(output, "o_res", o_res,
    cols = list(source_name = c("Source", "text"), share = c("Share of origin's GVA", "pct"),
                va_dependent = c("GVA dependent, EUR m", "eur1")),
    plot = function() {
      d <- head(o_res(), num_or(input$o_top, top_n))
      bars_h(d$source_name, d$share, x_title = "Share of the origin's GVA",
             caption = paste0(input$o_year, ". Origin GVA: ", fmt_bn(o_res()$origin_va[1]), "."))
    },
    height = function() bar_height(min(nrow(o_res()), num_or(input$o_top, top_n))),
    file_stub = "sources_of_origin"
  )

  # ---- 4. Top pairs -------------------------------------------------------------------------------------------------
  p_res <- reactive({
    need_sel(input$p_oc, input$p_os, input$p_sc, input$p_ss)
    r <- top_pairs(as.integer(input$p_year), input$p_oc, input$p_os, input$p_sc, input$p_ss,
                   type = input$p_type, foreign_only = input$p_foreign,
                   min_va = num_or(input$p_min_va, 0), n_top = num_or(input$p_n, 100))
    validate(need(nrow(r) > 0, "No pairs for this choice."))
    r[, pair := paste(origin, "←", source)]
  })
  wire_card(output, "p_res", p_res,
    cols = list(origin = c("Origin", "text"), source = c("Source", "text"),
                share = c("Share of origin's GVA", "pct"), va_dependent = c("GVA dependent, EUR m", "eur1"),
                origin_va = c("Origin GVA, EUR m", "eur")),
    plot = function() {
      d <- head(p_res(), 25)
      bars_h(d$pair, d$share, x_title = "Share of the origin's GVA",
             caption = paste0(input$p_year, ". Origin ← source. Top 25; the table has more."))
    },
    height = function() bar_height(min(nrow(p_res()), 25)),
    file_stub = "top_pairs"
  )

  # ---- 5. Shock simulator ---------------------------------------------------------------------------------------------
  shock_spec <- reactiveVal(NULL)
  observeEvent(input$s_run, {
    shock_spec(list(type = input$s_type, buyers = input$s_buyers, cats = input$s_cats,
                    pc = input$s_pc, ps = input$s_ps, src_c = input$s_src_c, src_s = input$s_src_s,
                    unit = input$s_unit, size = input$s_size, year = as.integer(input$s_year),
                    induced = isTRUE(input$s_induced), rate = input$s_rate))
  })

  s_impact <- reactive({
    sp <- shock_spec()
    validate(need(!is.null(sp), "Set up a shock on the left and press \"Run shock\"."))
    validate(need(is.numeric(sp$size) && !is.na(sp$size) && sp$size != 0, "Give a size other than 0."))
    size <- if (sp$unit == "pct") list(pct = sp$size) else list(eur_m = sp$size)
    size <- c(size, list(induced = isTRUE(sp$induced),
                         spend_rate = if (isTRUE(sp$induced)) spend_rate_or_stop(sp$rate) else household_spend_rate))
    tryCatch({
      if (sp$type == "final_demand") {
        need_sel(sp$buyers, sp$cats, sp$pc, sp$ps)
        do.call(fd_shock, c(list(year = sp$year, buyers = sp$buyers, categories = sp$cats,
                                 product_countries = sp$pc, product_sectors = sp$ps), size))
      } else {
        need_sel(sp$src_c, sp$src_s)
        do.call(output_shock, c(list(year = sp$year, source_countries = sp$src_c,
                                     source_sectors = sp$src_s), size))
      }
    }, error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  shock_text <- function(sp) {
    size <- if (sp$unit == "pct") sprintf("%+g%%", sp$size) else sprintf("%+g EUR m", sp$size)
    txt <- if (sp$type == "final_demand") {
      paste0("Final demand of ", describe(sp$buyers), " (", paste(fd_categories[sp$cats], collapse = ", "),
             ") for products of ", describe(sp$pc, sp$ps), ": ", size, ", ", sp$year)
    } else {
      paste0("Production of ", describe(sp$src_c, sp$src_s), ": ", size, ", ", sp$year)
    }
    if (isTRUE(sp$induced)) paste0(txt, ". ", induced_text(num_or(sp$rate, household_spend_rate))) else txt
  }
  output$s_desc <- renderText({ sp <- shock_spec(); req(sp); shock_text(sp) })
  s_world <- reactive(summarise_impact(s_impact(), "total"))
  s_shown <- reactive({ need_sel(input$s_show); summarise_impact(s_impact(), "total", input$s_show) })
  s_induced <- reactive(isTRUE(attr(s_impact(), "shock")$induced))
  output$s_size_out <- renderText(fmt_bn(attr(s_impact(), "shock")$size, 2))
  output$s_world    <- renderText(paste0(fmt_bn(s_world()$dva, 2), " (", fmt_pct(s_world()$share_va, 3), ")"))
  output$s_shown    <- renderText(paste0(fmt_bn(s_shown()$dva, 2), " (", fmt_pct(s_shown()$share_va, 3), ")"))
  induced_note <- function(s) if (s_induced()) paste("of which household spending:", fmt_bn(s$dva_induced, 2)) else ""
  output$s_world_note <- renderText(induced_note(s_world()))
  output$s_shown_note <- renderText(induced_note(s_shown()))

  is_production <- reactive(attr(s_impact(), "shock")$what == "production")
  s_countries <- reactive(summarise_impact(s_impact(), "country")[order(share_va)])
  wire_card(output, "s_country", s_countries,
    cols = function() {
      cols <- list(country_name = c("Country", "text"), share_va = c("Change, % of GVA", "pct"),
                   dva = c("Change in GVA, EUR m", "eur1"),
                   dva_excl_source = c("Of which outside the shocked producers, EUR m", "eur1"),
                   dva_direct = c("Direct, EUR m", "eur1"), dva_first = c("First-round suppliers, EUR m", "eur1"),
                   dva_further = c("Further supply-chain rounds, EUR m", "eur1"),
                   dva_induced = c("Household spending, EUR m", "eur1"),
                   va = c("GVA, EUR m", "eur"))
      if (!is_production()) cols$dva_excl_source <- NULL
      if (!s_induced()) cols$dva_induced <- NULL
      cols
    },
    plot = function() {
      d <- s_countries()
      if (input$s_country_scope == "shown") {
        d <- d[country %in% expand_countries(input$s_show)]
      } else {
        # The buyer countries (final demand) or producer countries (production) where the shock happens
        shocked <- if (is_production()) unique(s_impact()[in_source == TRUE, country]) else expand_countries(shock_spec()$buyers)
        rest_of_d <- d[!country %in% shocked]
        if (!isFALSE(input$s_excl_shocked) && nrow(rest_of_d) > 0) d <- rest_of_d
        d <- d[order(-abs(share_va))][1:min(.N, 20)]
      }
      d <- d[order(-abs(share_va))]
      bars_h(d$country_name, d$share_va, x_title = "Change in GVA, % of the country's GVA",
             highlight = if (input$s_country_scope == "world") d$country %in% expand_countries(input$s_show),
             highlight_names = c("Countries shown", "Other"))
    },
    height = function() bar_height(if (input$s_country_scope == "shown") length(expand_countries(input$s_show)) else 20),
    file_stub = "shock_by_country"
  )

  output$s_cs_parts <- renderText({
    t <- s_shown()
    parts_text("in the countries shown", t$dva, t$dva_direct, t$dva_first, t$dva_further, t$dva_induced,
               is_production(), s_induced())
  })
  s_cs <- reactive({
    need_sel(input$s_show)
    summarise_impact(s_impact(), "country_sector", input$s_show, num_or(input$s_min_va, 0))[order(share_va)]
  })
  wire_card(output, "s_cs", s_cs,
    cols = function() {
      cols <- list(label = c("Country-industry", "text"), share_va = c("Change, % of own GVA", "pct"),
                   dva = c("Change in GVA, EUR m", "eur1"),
                   dva_direct = c("Direct, EUR m", "eur1"), dva_first = c("First-round suppliers, EUR m", "eur1"),
                   dva_further = c("Further supply-chain rounds, EUR m", "eur1"),
                   dva_induced = c("Household spending, EUR m", "eur1"),
                   share_country_va = c("Change, % of country's GVA", "pct"), va = c("GVA, EUR m", "eur"))
      if (!s_induced()) cols$dva_induced <- NULL
      cols
    },
    plot = function() {
      d <- s_cs()
      if (is_production()) d <- d[in_source == FALSE]   # the shocked producers change by exactly the shock
      d <- head(d[order(-abs(share_va))], num_or(input$s_top, top_n))
      validate(need(nrow(d) > 0, "No country-industries above the GVA threshold."))
      effect_chart(d$label, d[, .(va, direct = dva_direct, first = dva_first, further = dva_further,
                                  induced = dva_induced)],
                   pct = TRUE, x_title = "Change in GVA, % of own GVA")
    },
    height = function() bar_height(num_or(input$s_top, top_n), extra = 135),
    file_stub = "shock_by_country_industry"
  )

  s_heat <- reactive({
    need_sel(input$s_show)
    summarise_impact(s_impact(), "country_sector_group", input$s_show)
  })
  s_heat_wide <- reactive({
    w <- dcast(s_heat(), sector_group ~ country, value.var = "share_va")
    w[match(unique(sectors$group), sector_group)][!is.na(sector_group)]
  })
  wire_card(output, "s_heat", s_heat_wide,
    cols = function() c(list(sector_group = c("Industry group", "text")),
                        setNames(lapply(expand_countries(input$s_show), function(k) c(k, "pct")),
                                 expand_countries(input$s_show))),
    plot = function() {
      d <- s_heat()[, .(row = sector_group, col = country, value = share_va)]
      heatmap_t(as.data.frame(d), unique(sectors$group), expand_countries(input$s_show), digits = 2,
                caption = "Values in % of the group's GVA in each country.")
    },
    height = function() 420,
    file_stub = "shock_by_industry_group"
  )

  output$s_dl_all <- downloadHandler(
    filename = function() paste0("shock_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      cs_all <- summarise_impact(s_impact(), "country_sector")
      writexl::write_xlsx(list(
        shock               = data.frame(shock = shock_text(shock_spec())),
        by_country          = as.data.frame(s_countries()),
        by_country_industry = as.data.frame(cs_all[, .(country, country_name, sector, sector_name, va, dva, dva_direct, dva_first, dva_further, dva_induced,
                                                       share_va, share_country_va, in_source)]),
        by_group_shown      = as.data.frame(s_heat())
      ), file)
    }
  )

  # ---- 6. Domestic shock ----------------------------------------------------------------------------------------------
  # A sensible size when the unit changes (keeps the sign)
  observeEvent(input$h_unit, {
    sgn <- if (isTRUE(input$h_size < 0)) -1 else 1
    updateNumericInput(session, "h_size", value = sgn * if (input$h_unit == "pct") 10 else 1000)
  }, ignoreInit = TRUE)

  h_year    <- reactive(as.integer(input$h_year))
  h_induced <- reactive(isTRUE(input$h_induced))
  h_rate    <- reactive(if (h_induced()) spend_rate_or_stop(input$h_rate) else household_spend_rate)
  h_impact <- reactive({
    validate(need(is.numeric(input$h_size) && !is.na(input$h_size) && input$h_size != 0,
                  "Give a size other than 0."))
    size <- if (input$h_unit == "pct") list(pct = input$h_size) else list(eur_m = input$h_size)
    size <- c(size, list(induced = h_induced(), spend_rate = h_rate()))
    tryCatch({
      if (input$h_type == "final_demand") {
        need_sel(input$h_cats, input$h_fd_s)
        do.call(domestic_shock, c(list(year = h_year(), country = input$h_c, type = "final_demand",
                                       categories = input$h_cats, sectors = input$h_fd_s,
                                       products_from = input$h_from), size))
      } else {
        need_sel(input$h_prod_s)
        do.call(domestic_shock, c(list(year = h_year(), country = input$h_c, type = "production",
                                       sectors = input$h_prod_s), size))
      }
    }, error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  h_is_production <- reactive(attr(h_impact(), "shock")$what == "production")
  h_head <- reactive(home_vs_abroad(h_impact(), input$h_c))

  output$h_desc <- renderText({
    size <- if (input$h_unit == "pct") sprintf("%+g%%", input$h_size) else sprintf("%+g EUR m", input$h_size)
    who  <- country_names[[input$h_c]]
    txt <- if (input$h_type == "final_demand") {
      paste0(who, ": final demand of home buyers (", paste(fd_categories[input$h_cats], collapse = ", "), ") for ",
             if (input$h_from == "home") "home-made" else "home-made and imported", " products of ",
             describe_sectors(input$h_fd_s), ": ", size, ", ", input$h_year, ".")
    } else {
      paste0(who, ": production of ", describe_sectors(input$h_prod_s), ": ", size, ", ", input$h_year, ".")
    }
    if (h_induced()) paste(txt, induced_text(h_rate())) else txt
  })
  eur_per <- function(x) sprintf("EUR %.2f", x)
  output$h_size_out    <- renderText(fmt_bn(h_head()$shock, 2))
  output$h_size_note   <- renderText(if (h_is_production()) "output of the shocked home industries"
                                     else "final demand, basic prices")
  output$h_home        <- renderText(fmt_bn(h_head()$dva_home, 2))
  output$h_home_note   <- renderText({
    h <- h_head()
    txt <- paste(fmt_pct(h$share_home_va, 3), "of the country's GVA")
    if (h_induced()) paste0(txt, "; of which household spending: ", fmt_bn(h$dva_home_induced, 2)) else txt
  })
  output$h_mult_out    <- renderText(eur_per(h_head()$home_per_eur))
  output$h_mult_note   <- renderText({
    h <- h_head()
    if (h_induced()) paste("without household spending:", eur_per(h$home_per_eur_type1))
    else if (h_is_production()) paste("of which other home industries:", eur_per(h$dva_home_other / h$shock))
    else paste0("abroad: ", eur_per(h$abroad_per_eur), "; the rest is taxes on products")
  })
  output$h_abroad_out  <- renderText(fmt_bn(h_head()$dva_abroad, 2))
  output$h_abroad_note <- renderText(paste(fmt_pct(h_head()$leakage), "of the change in GVA everywhere"))

  h_ind <- reactive({
    lvl  <- input$h_level
    prod <- h_is_production()
    s <- summarise_impact(h_impact(), lvl, input$h_c, num_or(input$h_min_va, 0))
    if (prod && lvl == "sector") s <- s[in_source == FALSE]
    # In a production shock only the change outside the shocked industries is shown: no direct part
    s[, `:=`(change  = if (prod) dva_excl_source else dva,
             direct  = if (prod) 0 else dva_direct,
             first   = dva_first,
             further = dva_further,
             induced = if (prod) dva_induced_excl_source else dva_induced)]
    s[, share := change / va]
    s[, name := if (lvl == "sector") sector_name else sector_group]
    s[order(-abs(if (input$h_metric == "share") share else change))]
  })
  output$h_ind_parts <- renderText({
    h <- h_head()
    parts_text(paste("in", country_names[[input$h_c]]), h$dva_home, h$dva_home_direct, h$dva_home_first,
               h$dva_home_further, h$dva_home_induced, h_is_production(), h_induced())
  })
  wire_card(output, "h_ind", h_ind,
    cols = function() {
      cols <- list(name = c(name_col(input$h_level), "text"), change = c("Change in GVA, EUR m", "eur1"),
                   direct = c("Direct, EUR m", "eur1"), first = c("First-round suppliers, EUR m", "eur1"),
                   further = c("Further supply-chain rounds, EUR m", "eur1"),
                   induced = c("Household spending, EUR m", "eur1"),
                   share = c("Change, % of own GVA", "pct"), va = c("GVA, EUR m", "eur"))
      if (h_is_production()) cols$direct <- NULL
      if (!h_induced()) cols$induced <- NULL
      cols
    },
    plot = function() {
      d <- head(h_ind(), num_or(input$h_top, top_n))
      validate(need(nrow(d) > 0, "No home industries above the GVA threshold."))
      pct <- input$h_metric == "share"
      effect_chart(d$name, d, pct, x_title = if (pct) "Change in GVA, % of own GVA" else "Change in GVA, EUR m",
                   caption = paste0(country_names[[input$h_c]], ", ", input$h_year, "."))
    },
    height = function() bar_height(min(nrow(h_ind()), num_or(input$h_top, top_n)), extra = 135),
    file_stub = "domestic_shock_home_industries"
  )

  h_abroad <- reactive({
    s <- summarise_impact(h_impact(), "country")[country != input$h_c]
    s[, share_abroad := dva / sum(dva)]
    s[order(-abs(dva))]
  })
  wire_card(output, "h_abroad", h_abroad,
    cols = list(country_name = c("Country", "text"), dva = c("Change in GVA, EUR m", "eur1"),
                share_abroad = c("Share of the change abroad", "pct"),
                share_va = c("Change, % of the country's GVA", "pct"), va = c("GVA, EUR m", "eur")),
    plot = function() {
      d <- head(h_abroad(), num_or(input$h_top, top_n))
      bars_h(d$country_name, d$dva, x_title = "Change in GVA, EUR m", percent = FALSE,
             highlight = d$country %in% expand_countries(default_focus),
             highlight_names = c(default_focus, "Other countries"),
             caption = paste0("All countries abroad: ", fmt_m(h_head()$dva_abroad), ", ",
                              fmt_pct(h_head()$leakage), " of the change in GVA everywhere."))
    },
    height = function() bar_height(min(nrow(h_abroad()), num_or(input$h_top, top_n))),
    file_stub = "domestic_shock_abroad"
  )

  # Structure of the home economy (no shock needed)
  h_expo <- reactive({
    lvl <- input$h_level
    e <- home_demand_exposure(h_year(), input$h_c, lvl)
    e[, col := home_demand_short[code]]
    sizes <- unique(e[, .(origin_name, va)])[order(-va)]
    if (lvl == "sector") sizes <- sizes[va >= num_or(input$h_min_va, 0)]
    rows <- if (lvl == "sector") head(sizes$origin_name, num_or(input$h_top, top_n)) else sizes$origin_name
    list(long = e[origin_name %in% sizes$origin_name], rows = rows, all_rows = sizes$origin_name,
         cols = unname(home_demand_short))
  })
  h_expo_wide <- reactive({
    h <- h_expo()
    w <- dcast(h$long, origin_name + va ~ col, value.var = "share")
    w[match(h$all_rows, origin_name), c("origin_name", "va", h$cols), with = FALSE]
  })
  wire_card(output, "h_expo", h_expo_wide,
    cols = function() c(list(origin_name = c(name_col(input$h_level), "text"), va = c("GVA, EUR m", "eur")),
                        setNames(lapply(h_expo()$cols, function(k) c(k, "pct")), h_expo()$cols)),
    plot = function() {
      h <- h_expo()
      d <- h$long[origin_name %in% h$rows, .(row = origin_name, col, value = share)]
      heatmap_t(as.data.frame(d), h$rows, h$cols, dodge = 2,
                caption = paste0(country_names[[input$h_c]], ", ", input$h_year, ". % of the row's GVA. ",
                                 if (input$h_level == "sector") "Largest industries by GVA; the table has all." else ""))
    },
    height = function() 140 + 30 * length(h_expo()$rows),
    file_stub = "home_industries_by_demand"
  )

  # With household spending on, the charts get one more (green) part: the extra home GVA it brings,
  # on top of the EUR 1
  h_content <- reactive(domestic_content(h_year(), input$h_c, h_induced(), h_rate()))
  content_parts <- c(home_per_eur = "Home GVA", abroad_per_eur = "GVA abroad", other_per_eur = "Taxes on products & other")
  wire_card(output, "h_content", h_content,
    cols = function() {
      cols <- list(category = c("Final demand", "text"), final_demand = c("Final demand, EUR m", "eur"),
                   va_home = c("Home GVA, EUR m", "eur"), home_per_eur = c("Home GVA, % of each EUR", "pct"),
                   abroad_per_eur = c("GVA abroad, % of each EUR", "pct"),
                   other_per_eur = c("Taxes on products & other, % of each EUR", "pct"),
                   home_hh_per_eur = c("Extra home GVA from household spending, % of each EUR", "pct"))
      if (!h_induced()) cols$home_hh_per_eur <- NULL
      cols
    },
    plot = function() {
      d <- h_content()[code != "P5M"]
      parts <- content_parts; colours <- content_colours
      if (h_induced()) {
        parts <- c(parts, home_hh_per_eur = hh_label)
        colours <- c(colours, setNames(hh_green, hh_label))
      }
      long <- rbindlist(lapply(names(parts), function(p)
        data.table(label = d$category, category = parts[[p]], value = d[[p]])))
      stacked_h(as.data.frame(long), x_title = "Share of each EUR of final demand", colours = colours,
                caption = paste0(country_names[[input$h_c]], ", ", input$h_year, "."))
    },
    height = function() bar_height(5, per_bar = 36, extra = 120),
    file_stub = "home_demand_content"
  )
  output$h_content_time <- renderPlot({
    req(input$h_content_tabs == "Over time")
    ind <- h_induced()
    d <- if (ind) {
      withProgress(message = "Computing all years", value = 0, {
        rbindlist(lapply(yrs, function(y) {
          incProgress(1 / length(yrs))
          domestic_content(y, input$h_c, TRUE, h_rate())
        }))
      })
    } else {
      domestic_content(yrs, input$h_c)
    }
    d <- d[code != "P5M", .(year, series = category,
                            value = home_per_eur + if (ind) home_hh_per_eur else 0)]
    lines_t(d, levels = c(unname(fd_categories[c("P3_S14", "P3_S13", "P3_S15", "P51G")]), "All final demand"),
            y_title = "Home GVA, % of each EUR",
            caption = paste0(country_names[[input$h_c]], ". ",
                             if (ind) induced_text(h_rate()) else "Without household spending of extra wages."))
  }, height = 440, res = 96)

  h_mult <- reactive({
    m <- domestic_multipliers(h_year(), input$h_c, h_induced(), h_rate())[va >= num_or(input$h_min_va, 0) & !is.na(va_home)]
    m[, total_home := va_home + if (h_induced()) va_home_hh else 0]
    if (input$h_mult_order == "high") m[order(-total_home)] else m[order(total_home)]
  })
  mult_parts <- c(va_own = "The industry itself", va_other_home = "Other home industries",
                  va_abroad = "GVA abroad", other = "Taxes on products & other")
  wire_card(output, "h_mult", h_mult,
    cols = function() {
      cols <- list(sector_name = c("Industry", "text"), va_home = c("Home GVA, % of each EUR", "pct"),
                   va_own = c("Of which the industry itself", "pct"),
                   va_other_home = c("Of which other home industries", "pct"),
                   va_abroad = c("GVA abroad, % of each EUR", "pct"), other = c("Taxes & other, % of each EUR", "pct"),
                   va_home_hh = c("Extra home GVA from household spending, % of each EUR", "pct"),
                   output_multiplier = c("Home output per EUR (supply chain only)", "dec"),
                   final_demand = c("Final demand for its products, EUR m", "eur"), va = c("GVA, EUR m", "eur"))
      if (!h_induced()) cols$va_home_hh <- NULL
      cols
    },
    plot = function() {
      d <- head(h_mult(), num_or(input$h_top, top_n))
      validate(need(nrow(d) > 0, "No home industries above the GVA threshold."))
      parts <- mult_parts; colours <- multiplier_colours
      if (h_induced()) {
        parts <- c(parts, va_home_hh = hh_label)
        colours <- c(colours, setNames(hh_green, hh_label))
      }
      long <- rbindlist(lapply(names(parts), function(p)
        data.table(label = d$sector_name, category = parts[[p]], value = d[[p]])))
      stacked_h(as.data.frame(long), x_title = "Share of each EUR of final demand",
                colours = colours, legend_rows = if (h_induced()) 3 else 2,
                caption = paste0(country_names[[input$h_c]], ", ", input$h_year, ". Industries with GVA of at least ",
                                 fmt_m(num_or(input$h_min_va, 0)), "."))
    },
    height = function() bar_height(min(nrow(h_mult()), num_or(input$h_top, top_n)), extra = if (h_induced()) 145 else 120),
    file_stub = "home_multipliers"
  )

  # ---- 7. Government --------------------------------------------------------------------------------------------------
  g_shares <- reactive({
    need_sel(input$g_show)
    demand_shares(as.integer(input$g_year), input$g_show, "P3_S13")[order(-(share_domestic + share_foreign))]
  })
  gov_colours <- c("Home government" = blue, "Foreign governments" = "#eb6834")
  wire_card(output, "g_share", g_shares,
    cols = list(country_name = c("Country", "text"), share_domestic = c("Home government", "pct"),
                share_foreign = c("Foreign governments", "pct"), va_domestic = c("Home, EUR m", "eur"),
                va_foreign = c("Foreign, EUR m", "eur"), va = c("GVA, EUR m", "eur")),
    plot = function() {
      g <- g_shares()
      d <- rbind(data.frame(label = g$country_name, category = "Home government", value = g$share_domestic),
                 data.frame(label = g$country_name, category = "Foreign governments", value = g$share_foreign))
      stacked_h(d, x_title = "Share of the country's GVA", colours = gov_colours,
                caption = paste0(input$g_year, "."))
    },
    height = function() bar_height(nrow(g_shares())),
    file_stub = "government_share"
  )

  g_time <- reactive({
    need_sel(input$g_show)
    d <- demand_shares(yrs, input$g_show, "P3_S13")
    d[, value := switch(input$g_time_measure, foreign = share_foreign, home = share_domestic,
                        both = share_foreign + share_domestic)]
    d[, panel := factor(country_name, levels = g_shares()$country_name)]
    d
  })
  g_time_wide <- reactive(dcast(g_time(), country_name ~ year, value.var = "value"))
  wire_card(output, "g_time", g_time_wide,
    cols = c(list(country_name = c("Country", "text")),
             setNames(lapply(as.character(yrs), function(y) c(y, "pct")), as.character(yrs))),
    plot = function() {
      lbl <- c(foreign = "foreign governments", home = "home government", both = "all governments")[[input$g_time_measure]]
      small_multiples(as.data.frame(g_time()), y_title = paste("Share of GVA, depends on", lbl))
    },
    height = function() 170 * ceiling(length(unique(g_time()$country)) / 4) + 60,
    file_stub = "government_share_over_time"
  )

  output$g_benef_title <- renderText(paste0("Who benefits from the consumption of the government of ",
                                            country_names[[input$g_gov]], " (outside ", input$g_gov, ")"))
  g_benef <- reactive({
    show <- setdiff(expand_countries(input$g_show), input$g_gov)
    validate(need(length(show) > 0, "Choose countries other than the government's own."))
    imp <- va_from_final_demand(as.integer(input$g_year), buyers = input$g_gov, categories = "P3_S13")
    s <- summarise_impact(imp, input$g_level, show, num_or(input$g_min_va, 0))
    s[, name := switch(input$g_level, country = country_name, country_sector = label, sector = sector_name)]
    if (input$g_metric == "share") s[order(-share_va)] else s[order(-dva)]
  })
  wire_card(output, "g_benef", g_benef,
    cols = list(name = c("Who", "text"), share_va = c("Share of own GVA", "pct"),
                dva = c("GVA from this government, EUR m", "eur1"), va = c("GVA, EUR m", "eur")),
    plot = function() {
      d <- head(g_benef(), num_or(input$g_top, top_n))
      if (input$g_metric == "share") {
        bars_h(d$name, d$share_va, x_title = "Share of own GVA")
      } else {
        bars_h(d$name, d$dva, x_title = "GVA from this government, EUR m", percent = FALSE)
      }
    },
    height = function() bar_height(min(nrow(g_benef()), num_or(input$g_top, top_n))),
    file_stub = "government_beneficiaries"
  )

  # Send the scenario to the simulator and run it there
  observeEvent(input$g_sim, {
    updateRadioButtons(session, "s_type", selected = "final_demand")
    updateSelectizeInput(session, "s_buyers", selected = input$g_gov)
    updateCheckboxGroupInput(session, "s_cats", selected = "P3_S13")
    updateSelectizeInput(session, "s_pc", selected = "World")
    updateSelectizeInput(session, "s_ps", selected = "All sectors")
    updateRadioButtons(session, "s_unit", selected = "eur")
    updateNumericInput(session, "s_size", value = input$g_amount)
    updateSelectInput(session, "s_year", selected = input$g_year)
    updateSelectizeInput(session, "s_show", selected = input$g_show)
    shock_spec(list(type = "final_demand", buyers = input$g_gov, cats = "P3_S13", pc = "World",
                    ps = "All sectors", src_c = NULL, src_s = NULL, unit = "eur",
                    size = input$g_amount, year = as.integer(input$g_year),
                    induced = isTRUE(input$s_induced), rate = input$s_rate))   # keeps the simulator's setting
    nav_select("tabs", "sim")
  })
}

shinyApp(ui, server)
