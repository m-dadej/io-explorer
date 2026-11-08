# =============================================================================
# The guide box at the top of each tab, read from app/guide.md. Used by the app
# and by R/04_export_web_text.R (which writes it for the website).
# =============================================================================

# Returns a list by tab id, each with: question (text), intro and details (HTML).
# The markdown format is described at the top of guide.md.
read_guide <- function(path) {
  txt   <- paste(readLines(path, encoding = "UTF-8"), collapse = "\n")
  txt   <- gsub("<!--.*?-->", "", txt, perl = TRUE)            # the comment at the top
  parts <- strsplit(txt, "(^|\n)## ", perl = TRUE)[[1]][-1]    # one part per tab
  md    <- function(x) commonmark::markdown_html(x, extensions = TRUE)
  out <- lapply(parts, function(p) {
    head   <- sub("\n.*", "", p)
    body   <- sub("^[^\n]*\n?", "", p)
    halves <- strsplit(body, "\n### How to read it[^\n]*\n", perl = TRUE)[[1]]
    list(id       = trimws(sub("\\|.*", "", head)),
         question = trimws(sub("^[^|]*\\|", "", head)),
         intro    = md(halves[1]),
         details  = if (length(halves) > 1) md(halves[2]) else "")
  })
  setNames(out, vapply(out, `[[`, "", "id"))
}
