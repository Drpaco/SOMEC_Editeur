suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(janitor)
  library(openxlsx)
})

# ============================================================
# CONFIG — mirrors interactive_mission_qc_console.R paths
# ============================================================

.repo_root <- tryCatch({
  frames <- sys.frames()
  ofiles <- Filter(Negate(is.null), lapply(frames, function(f) f$ofile))
  if (length(ofiles) > 0) {
    dirname(normalizePath(ofiles[[1]], mustWork = TRUE))
  } else {
    dirname(normalizePath(
      rstudioapi::getSourceEditorContext()$path, mustWork = TRUE
    ))
  }
}, error = function(e) getwd())

# .accdb files live 2 levels up from .repo_root
db_dir <- normalizePath(dirname(dirname(.repo_root)), winslash = "/", mustWork = FALSE)

# pick latest SOMEC_YYYYMMDD.accdb
accdb_files <- list.files(db_dir, pattern = "^SOMEC_\\d{8}\\.accdb$", full.names = TRUE)
if (!length(accdb_files)) stop("No SOMEC_YYYYMMDD.accdb found in: ", db_dir)
accdb_path <- accdb_files[order(file.info(accdb_files)$mtime, decreasing = TRUE)][1]
accdb_tag  <- sub(".*SOMEC_(\\d{8})\\.accdb$", "\\1", accdb_path)

cfg <- list(
  accdb_path          = accdb_path,
  accdb_tag           = accdb_tag,
  context_dir         = file.path(.repo_root, "GlobalContext"),
  mission_reports_dir = file.path(.repo_root, "MissionReports"),
  cache_dir           = file.path(.repo_root, "GlobalContext", "_cache")
)

dir.create(cfg$cache_dir,           showWarnings = FALSE, recursive = TRUE)
dir.create(cfg$mission_reports_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# MODULES
# ============================================================

source(file.path(.repo_root, "qc_helpers.R"))
source(file.path(.repo_root, "qc_cross_validation.R"))

# ============================================================
# LOAD DATA
# ============================================================

missions     <- load_access_table_cached(cfg$accdb_path, "missions",     cfg$cache_dir) |> clean_names()
transects    <- load_access_table_cached(cfg$accdb_path, "transects",    cfg$cache_dir) |> clean_names()
observations <- load_access_table_cached(cfg$accdb_path, "observations", cfg$cache_dir) |> clean_names()

# ============================================================
# LOAD PROFILER ISSUES
# ============================================================

mi_path <- file.path(cfg$mission_reports_dir, "mission_issues.rds")
if (!file.exists(mi_path)) stop("mission_issues.rds not found. Run the profiler first.")
mission_issues <- readRDS(mi_path)

# ============================================================
# 1) MISSION_ISSUES COUNTS (profiler flags)
# ============================================================

mi_long <- mission_issues |>
  count(mission, source = issue_type, name = "n")

# ============================================================
# 2) CROSS-VALIDATION COUNTS (R1..R11)
# ============================================================

cat("Running cross-validation for all missions...\n")
cv_long <- missions |>
  distinct(mission) |>
  pull(mission) |>
  imap_dfr(function(m, i) {
    if (i %% 10 == 0) cat(sprintf("  %d / %d\n", i, nrow(missions)))
    cv <- tryCatch(run_cross_validation(m), error = function(e) list())
    if (length(cv) == 0) return(tibble(mission = m, source = NA_character_, n = 0L))
    imap_dfr(cv, ~ tibble(mission = m, source = .y, n = nrow(.x)))
  }) |>
  filter(!is.na(source))

# ============================================================
# 3) COMBINED LONG TABLE
# ============================================================

mission_error_long <- bind_rows(mi_long, cv_long) |>
  group_by(mission, source) |>
  summarise(n = sum(n), .groups = "drop") |>
  arrange(mission, desc(n))

# ============================================================
# 4) WIDE SUMMARY + IMPORT SUSPICION FLAG
# ============================================================

summary_base <- mission_error_long |>
  group_by(mission) |>
  summarise(
    total_potential_errors = sum(n),
    n_error_types          = n_distinct(source),
    .groups = "drop"
  )

wide <- mission_error_long |>
  pivot_wider(names_from = source, values_from = n, values_fill = 0L)

# adaptive threshold: flag missions in top 5%
thr_total <- quantile(summary_base$total_potential_errors, probs = 0.95, na.rm = TRUE)

mission_error_summary <- summary_base |>
  left_join(wide, by = "mission") |>
  mutate(
    n_unknown_catalog = coalesce(UNKNOWN_CATALOG,        0L),
    n_numeric_outlier = coalesce(NUMERIC_OUTLIER,        0L),
    n_datetime        = coalesce(OUTSIDE_MISSION_DATES,  0L),
    n_r10             = coalesce(R10,                    0L),
    n_r11             = coalesce(R11,                    0L),
    import_issue_suspected =
      total_potential_errors >= thr_total |
      n_unknown_catalog      >= 20        |
      n_r11                  >= 3,
    import_signal = case_when(
      total_potential_errors >= thr_total ~ "very_long_issue_list",
      n_r11                  >= 3         ~ "transect_endpoint_missing_or_duplicate",
      n_unknown_catalog      >= 20        ~ "many_unknown_catalog_values",
      TRUE                               ~ NA_character_
    )
  ) |>
  arrange(desc(import_issue_suspected), desc(total_potential_errors), mission)

# ============================================================
# 5) EXCEL OUTPUT
# ============================================================

out_xlsx <- file.path(cfg$mission_reports_dir,
                      paste0("mission_error_summary_", cfg$accdb_tag, ".xlsx"))

wb           <- createWorkbook()
orange_fill  <- createStyle(fgFill = "#FFA500")
bold_header  <- createStyle(textDecoration = "bold", wrapText = TRUE)

# ---- Sheet 1: Summary (wide)
addWorksheet(wb, "Summary")
writeDataTable(wb, "Summary", mission_error_summary, tableStyle = "TableStyleMedium2")
setColWidths(wb, "Summary", cols = 1,                             widths = 16)
setColWidths(wb, "Summary", cols = 2:3,                           widths = 14)
setColWidths(wb, "Summary", cols = 4:ncol(mission_error_summary), widths = 10)
flag_rows <- which(mission_error_summary$import_issue_suspected) + 1L
if (length(flag_rows) > 0) {
  addStyle(wb, "Summary", style = orange_fill,
           rows = flag_rows, cols = 1:ncol(mission_error_summary),
           gridExpand = TRUE, stack = TRUE)
}
addStyle(wb, "Summary", style = bold_header,
         rows = 1, cols = 1:ncol(mission_error_summary),
         gridExpand = TRUE, stack = TRUE)
freezePane(wb, "Summary", firstRow = TRUE, firstCol = TRUE)

# ---- Sheet 2: Long format
addWorksheet(wb, "Long")
writeDataTable(wb, "Long", mission_error_long, tableStyle = "TableStyleMedium6")
setColWidths(wb, "Long", cols = 1:3, widths = c(16, 28, 8))
addStyle(wb, "Long", style = bold_header,
         rows = 1, cols = 1:3, gridExpand = TRUE, stack = TRUE)
freezePane(wb, "Long", firstRow = TRUE)

# ---- Sheet 3: Import suspected only
suspected <- mission_error_summary |> filter(import_issue_suspected)
addWorksheet(wb, "ImportSuspected")
if (nrow(suspected) > 0) {
  writeDataTable(wb, "ImportSuspected", suspected, tableStyle = "TableStyleMedium3")
  setColWidths(wb, "ImportSuspected", cols = 1,                 widths = 16)
  setColWidths(wb, "ImportSuspected", cols = 2:3,               widths = 14)
  setColWidths(wb, "ImportSuspected", cols = 4:ncol(suspected), widths = 10)
  addStyle(wb, "ImportSuspected", style = orange_fill,
           rows = seq_len(nrow(suspected)) + 1L,
           cols = 1:ncol(suspected),
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "ImportSuspected", style = bold_header,
           rows = 1, cols = 1:ncol(suspected),
           gridExpand = TRUE, stack = TRUE)
  freezePane(wb, "ImportSuspected", firstRow = TRUE, firstCol = TRUE)
} else {
  writeData(wb, "ImportSuspected", "No missions flagged for import issues.")
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
cat("Saved:", out_xlsx, "\n")

# ============================================================
# 6) RETURN (useful objects in session)
# ============================================================

list(
  mission_error_summary = mission_error_summary,
  mission_error_long    = mission_error_long,
  out_xlsx              = out_xlsx
)