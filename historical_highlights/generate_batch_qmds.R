# Generates one report .qmd per cruise in data/keys_checkpoints_flat.csv from
# historical_highlights/template.qmd. Runs as a project pre-render step (see
# _quarto.yml); modeled on https://github.com/7yl4r/quartobatch

BATCH_NAME <- "historical_highlights"
TEMPLATE_REPLACEMENTS_KEY <- c(
  "cruise_id", "label", "window_start", "window_end", "regions", "n_stations"
)

REPORT_TEMPLATE <- here::here(glue::glue("{BATCH_NAME}/template.qmd"))
REPORTS_DIR <- here::here(glue::glue("{BATCH_NAME}/batched_reports"))

if (!nzchar(system.file(package = "librarian"))) {
  install.packages("librarian")
}
librarian::shelf(dplyr, glue, here, whisker, readr)

source(here::here("R", "cruise_reports.R"))

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

# `quarto preview` watches the whole project tree to decide when to
# live-reload, so rewriting every report file on every pre-render pass
# (even when nothing changed) creates filesystem churn that can race with
# its own in-progress render of unrelated files. Only touch a file when its
# content actually changes.
create_report <- function(params) {
  out_file <- file.path(REPORTS_DIR, glue("{params$cruise_id}.qmd"))
  rendered <- whisker::whisker.render(templ, params)

  previous <- if (file.exists(out_file)) {
    paste(readLines(out_file, warn = FALSE), collapse = "\n")
  } else {
    NA_character_
  }

  if (!identical(rendered, previous)) {
    print(glue("=== creating cruise report for '{params$cruise_id}' ==="))
    writeLines(rendered, out_file)
  }

  basename(out_file)
}

checkpoints <- read_cruise_checkpoints()
cruise_ids <- list_cruise_ids(checkpoints)
current_files <- vapply(cruise_ids, function(cruise_id_value) {
  info <- cruise_summary(cruise_id_value, checkpoints)
  create_report(list(
    cruise_id = info$cruise_id,
    label = info$label,
    window_start = format(info$window_start, "%Y-%m-%d"),
    window_end = format(info$window_end, "%Y-%m-%d"),
    regions = paste(info$regions, collapse = ", "),
    n_stations = nrow(info$stations)
  ))
}, character(1))

# remove reports for cruises no longer in the checkpoints CSV
stale_files <- setdiff(list.files(REPORTS_DIR, pattern = "\\.qmd$"), current_files)
if (length(stale_files) > 0) {
  unlink(file.path(REPORTS_DIR, stale_files))
}
