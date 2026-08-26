# Generates one report .qmd per cruise in data/keys_checkpoints_flat.csv from
# historical_highlights/template.qmd. Runs as a project pre-render step (see
# _quarto.yml); modeled on https://github.com/7yl4r/quartobatch

BATCH_NAME <- "historical_highlights"
TEMPLATE_REPLACEMENTS_KEY <- c(
  "cruise_id", "window_start", "window_end", "regions", "n_stations"
)

REPORT_TEMPLATE <- here::here(glue::glue("{BATCH_NAME}/template.qmd"))
REPORTS_DIR <- here::here(glue::glue("{BATCH_NAME}/batched_reports"))

if (!nzchar(system.file(package = "librarian"))) {
  install.packages("librarian")
}
librarian::shelf(dplyr, glue, here, whisker, readr)

source(here::here("R", "cruise_reports.R"))

if (dir.exists(REPORTS_DIR)) {
  unlink(REPORTS_DIR, recursive = TRUE)
}
dir.create(REPORTS_DIR, showWarnings = FALSE, recursive = TRUE)

# the template.qmd file uses EXAMPLE_<FIELD> placeholders (all-caps so they
# read cleanly as literal text in a standalone template) which get swapped
# for whisker {{field}} tags before rendering per cruise
templ_lines <- readLines(REPORT_TEMPLATE)
for (key in TEMPLATE_REPLACEMENTS_KEY) {
  templ_lines <- gsub(
    paste0("EXAMPLE_", toupper(key)),
    paste0("{{", key, "}}"),
    templ_lines,
    fixed = TRUE
  )
}
templ <- paste(templ_lines, collapse = "\n")

create_report <- function(params) {
  print(glue("=== creating cruise report for '{params$cruise_id}' ==="))
  rendered <- whisker::whisker.render(templ, params)
  writeLines(
    rendered,
    file.path(REPORTS_DIR, glue("{params$cruise_id}.qmd"))
  )
}

checkpoints <- read_cruise_checkpoints()
for (cruise_id_value in list_cruise_ids(checkpoints)) {
  info <- cruise_summary(cruise_id_value, checkpoints)
  create_report(list(
    cruise_id = info$cruise_id,
    window_start = format(info$window_start, "%Y-%m-%d"),
    window_end = format(info$window_end, "%Y-%m-%d"),
    regions = paste(info$regions, collapse = ", "),
    n_stations = nrow(info$stations)
  ))
}
