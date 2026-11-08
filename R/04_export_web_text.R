# =============================================================================
# The text of the website, from the app's markdown files:
#   docs/notes.html   the Guide & notes tab (from app/notes.md)
#   docs/guide.json   the guide box at the top of each tab (from app/guide.md)
#
# Quick and needs no data: 03_export_web.R runs it too. After editing the text, run it alone,
# from the project folder:
#   root_dir <- "."; source("R/04_export_web_text.R")
# =============================================================================

if (!exists("root_dir")) root_dir <- "."
source(file.path(root_dir, "app/guide.R"))

local({
  web_dir <- file.path(root_dir, "docs")
  notes <- commonmark::markdown_html(readLines(file.path(root_dir, "app/notes.md"), encoding = "UTF-8"),
                                     extensions = TRUE)
  writeLines(notes, file.path(web_dir, "notes.html"), useBytes = TRUE)
  guide <- read_guide(file.path(root_dir, "app/guide.md"))
  jsonlite::write_json(guide, file.path(web_dir, "guide.json"), auto_unbox = TRUE, pretty = TRUE)
  message("Wrote docs/notes.html and docs/guide.json (", length(guide), " tabs)")
})
