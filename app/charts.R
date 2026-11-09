# =============================================================================
# Chart style and chart functions used by the app
# Colours: one blue for a single series, grey for "the rest" when one group is
# highlighted, a fixed colour per final demand category, a blue ramp for heatmaps.
# =============================================================================

ink      <- "#0b0b0b"
ink_2    <- "#52514e"
muted    <- "#898781"
grid_col <- "#e1e0d9"
axis_col <- "#c3c2b7"
surface  <- "#ffffff"
blue     <- "#2a78d6"
rest     <- "#c9c7bf"   # bars that are not highlighted

# Each final demand category always has the same colour (government stands out in orange)
category_colours <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4"), fd_categories)

# Where EUR 1 of final demand ends up: home in blues, abroad in orange, taxes and other in grey
content_colours    <- c("Home GVA" = blue, "GVA abroad" = "#eb6834", "Taxes on products & other" = rest)
multiplier_colours <- c("The industry itself" = "#184f95", "Other home industries" = "#7fb3ee",
                        "GVA abroad" = "#eb6834", "Taxes on products & other" = rest)
# Household spending of extra wages always has this colour (green), wherever it is shown
hh_label <- "From household spending"   # extra home GVA, on top of the EUR 1
hh_green <- "#1baf7a"
# Parts of a shock's effect: supply-chain rounds in blues (darker = closer to the shock)
effect_colours <- c("Direct" = "#1a4f9c", "First-round suppliers" = "#3a80d8",
                    "Further supply-chain rounds" = "#7fb3ee", "Household spending" = hh_green)
# Parts of a price shock's cost push: bought directly from the producers whose price rises, or
# passed on by other suppliers
price_colours <- c("Direct purchases" = "#1a4f9c", "Through other suppliers" = "#7fb3ee")
# Prices by country: output prices (grey) next to the household basket (blue)
price_measures <- c("Output prices", "Household basket")
# Value added in exports: home in blue, foreign in orange, taxes and other in grey
export_colours <- c("Home GVA" = blue, "Foreign GVA" = "#eb6834", "Taxes on products & other" = rest)
# Trade shock: the change in GVA through sales of the country's own producers or of producers abroad
channel_colours <- c("Own country's sales" = "#1a4f9c", "Other countries' sales" = "#7fb3ee",
                     "Household spending" = hh_green)
# Industry profile: what each EUR of output pays for (GVA in blues, inputs in grey and orange)
cost_colours <- c("Compensation of employees" = "#184f95", "Gross operating surplus" = "#3a80d8",
                  "Other taxes on production" = "#a9cbf2", "Inputs from home industries" = "#8f8d86",
                  "Imported inputs" = "#eb6834", "Taxes on products & other" = "#d9d7cf")
# Industry profile: buyers (industries buying inputs, final buyers) and home vs foreign sources
buyer_colours <- c("Industries (inputs)" = "#184f95", "Final buyers" = "#7fb3ee")
home_colours  <- c(blue, "#eb6834")

# Colours for up to 8 lines, always in this order
series_colours <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")

theme_io <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor     = element_blank(),
    panel.grid.major     = element_line(colour = grid_col, linewidth = 0.3),
    axis.text            = element_text(colour = ink_2),
    axis.title           = element_text(colour = ink_2, size = 11),
    legend.position      = "top",
    legend.justification = "left",
    legend.location      = "plot",   # legend starts at the left edge of the chart, not of the bars
    legend.title         = element_blank(),
    legend.text          = element_text(colour = ink_2),
    plot.title.position  = "plot",
    plot.caption         = element_text(colour = muted, hjust = 0, size = 10),
    plot.caption.position = "plot",
    strip.text           = element_text(hjust = 0, face = "bold", colour = ink),
    plot.background      = element_rect(fill = surface, colour = NA),
    panel.background     = element_rect(fill = surface, colour = NA)
  )

pct_axis <- function(accuracy = NULL) scales::label_percent(accuracy = accuracy)

# Height in pixels for a chart with n horizontal bars
bar_height <- function(n_bars, per_bar = 24, extra = 90) max(220, extra + per_bar * n_bars)

# Horizontal bars, in the order given (first row on top).
# `highlight` (TRUE/FALSE per bar) paints TRUE bars blue and the others grey (or `highlight_colours`);
# `highlight_names` names the two groups in the legend.
bars_h <- function(labels, values, x_title = NULL, percent = TRUE, highlight = NULL,
                   highlight_names = c("Highlighted", "Other"), caption = NULL,
                   highlight_colours = c(blue, rest)) {
  d <- data.frame(label = factor(labels, levels = rev(unique(labels))), value = values)
  if (is.null(highlight)) {
    d$group <- "all"
    fills <- c(all = blue)
    show_legend <- FALSE
  } else {
    d$group <- factor(ifelse(highlight, highlight_names[1], highlight_names[2]), levels = highlight_names)
    fills <- setNames(highlight_colours, highlight_names)
    show_legend <- TRUE
  }
  ggplot(d, aes(value, label, fill = group)) +
    geom_col(width = 0.72) +
    geom_vline(xintercept = 0, colour = axis_col) +
    scale_fill_manual(values = fills, guide = if (show_legend) "legend" else "none") +
    scale_x_continuous(labels = if (percent) pct_axis() else scales::label_comma(),
                       expand = expansion(mult = c(0.02, 0.06)), position = "top") +
    labs(x = x_title, y = NULL, caption = caption) +
    theme_io +
    theme(panel.grid.major.y = element_blank())
}

# Horizontal stacked bars. `d` has columns label, category, value. By default the categories are
# the final demand categories with their fixed colours; pass `colours` (named, in stacking order)
# for other parts. percent = FALSE labels the axis in plain numbers (e.g. EUR m). legend_rows sets
# the rows of the legend (default: 1 for up to 3 parts, else 2).
stacked_h <- function(d, x_title = NULL, caption = NULL, colours = category_colours, percent = TRUE,
                      expand = c(0, 0.05), legend_rows = NULL) {
  if (is.null(legend_rows)) legend_rows <- if (length(colours) > 3) 2 else 1
  d$label    <- factor(as.character(d$label), levels = rev(unique(as.character(d$label))))
  d$category <- factor(d$category, levels = rev(names(colours)))
  ggplot(d, aes(value, label, fill = category)) +
    geom_col(width = 0.72, colour = surface, linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = axis_col) +
    scale_fill_manual(values = colours, breaks = names(colours), drop = TRUE) +
    scale_x_continuous(labels = if (percent) pct_axis() else scales::label_comma(),
                       expand = expansion(mult = expand), position = "top") +
    labs(x = x_title, y = NULL, caption = caption) +
    theme_io +
    theme(panel.grid.major.y = element_blank()) +
    guides(fill = guide_legend(nrow = legend_rows, byrow = TRUE))
}

# Two measures side by side per row (e.g. gross exports vs value added). `d` has label, measure, value;
# the first measure in `measures` is drawn grey, the second blue.
paired_h <- function(d, measures, x_title = NULL, caption = NULL) {
  d$label   <- factor(as.character(d$label), levels = rev(unique(as.character(d$label))))
  d$measure <- factor(d$measure, levels = rev(measures))
  ggplot(d, aes(value, label, fill = measure)) +
    geom_col(width = 0.76, position = position_dodge(width = 0.82)) +
    geom_vline(xintercept = 0, colour = axis_col) +
    scale_fill_manual(values = setNames(c(rest, blue), measures), breaks = measures) +
    scale_x_continuous(labels = pct_axis(), expand = expansion(mult = c(0, 0.05)), position = "top") +
    labs(x = x_title, y = NULL, caption = caption) +
    theme_io +
    theme(panel.grid.major.y = element_blank())
}

# Lines over time. `d` has year, series, value; series keep their colour in the order of `levels`.
lines_t <- function(d, levels = unique(d$series), y_title = NULL, percent = TRUE, caption = NULL) {
  levels <- head(levels, length(series_colours))
  d <- d[d$series %in% levels, ]
  d$series <- factor(d$series, levels = levels)
  ggplot(d, aes(year, value, colour = series)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.6) +
    scale_colour_manual(values = setNames(series_colours[seq_along(levels)], levels)) +
    scale_x_continuous(breaks = scales::breaks_pretty(8)) +
    scale_y_continuous(labels = if (percent) pct_axis() else scales::label_comma(),
                       limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = y_title, caption = caption) +
    theme_io +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
}

# One small chart per panel (e.g. per country), one blue line each, same y scale.
small_multiples <- function(d, y_title = NULL, caption = NULL, ncol = 4) {
  ggplot(d, aes(year, value)) +
    geom_line(linewidth = 0.8, colour = blue) +
    geom_point(size = 1.3, colour = blue) +
    facet_wrap(~panel, ncol = ncol) +
    scale_x_continuous(breaks = scales::breaks_pretty(4)) +
    scale_y_continuous(labels = pct_axis(), limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = y_title, caption = caption) +
    theme_io
}

# Heatmap: a coloured table. `d` has row, col, value (shares 0-1, may be negative for shocks:
# colour shows the size of the number, the label shows its sign). Columns in `plain_cols` show
# their numbers without colour (e.g. "Home", which would otherwise wash out the rest).
# dodge = 2 puts long column names on two alternating lines so they do not overlap.
heatmap_t <- function(d, row_levels, col_levels, caption = NULL, digits = 1, plain_cols = character(0),
                      dodge = 1) {
  d$row <- factor(d$row, levels = rev(row_levels))
  d$col <- factor(d$col, levels = col_levels)
  d$size <- ifelse(d$col %in% plain_cols, NA_real_, abs(d$value))
  cutoff <- 0.55 * max(d$size, na.rm = TRUE)
  d$size_hi <- !is.na(d$size) & d$size > cutoff
  d$text <- sprintf(paste0("%.", digits, "f"), round(100 * d$value, digits) + 0)   # + 0 turns -0 into 0
  ggplot(d, aes(col, row, fill = size)) +
    geom_tile(colour = surface, linewidth = 0.8) +
    geom_text(aes(label = text, colour = size_hi), size = 3.4) +
    scale_fill_gradient(low = "#eef5fd", high = "#184f95", guide = "none", na.value = "#f4f3ef") +
    scale_colour_manual(values = c(`TRUE` = "#ffffff", `FALSE` = ink), guide = "none") +
    scale_x_discrete(position = "top", guide = guide_axis(n.dodge = dodge)) +
    labs(x = NULL, y = NULL, caption = caption) +
    theme_io +
    theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 0))
}

# ---- Number formats ------------------------------------------------------------------------
fmt_pct <- function(x, digits = 1) ifelse(is.na(x), "-", sprintf(paste0("%.", digits, "f%%"), 100 * x))
fmt_bn  <- function(x, digits = 1) paste0("EUR ", formatC(x / 1000, format = "f", digits = digits, big.mark = ","), " bn")
fmt_m   <- function(x) paste0("EUR ", formatC(x, format = "f", digits = 0, big.mark = ","), " m")
