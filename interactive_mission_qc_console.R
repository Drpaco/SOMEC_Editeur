
# FILE: interactive_mission_qc_console.R
# PURPOSE:
#   Interactive console-based QA reviewer
#   - Categorical, datetime, and numeric QA
#   - Read-only: explains issues and PRINTS fix suggestions
#   - Optionally appends approved fixes to apply_qc_fixes_<version>.R
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(RODBC)
  library(stringdist)
})

# ============================================================
# LANGUAGE
# ============================================================

cat("Language / Langue: [e] English  [f] Français > ")
.lang <- tolower(trimws(readline()))
if (!.lang %in% c("e", "f")) .lang <- "e"

# Message lookup — msg("key") returns the string in the chosen language
.strings <- list(
  loading_cache     = c(e = "Loading cached version of",       f = "Chargement de la version en cache de"),
  no_accdb          = c(e = "not found.",                       f = "introuvable."),
  session_complete  = c(e = "QC session complete.",             f = "Session de contrôle qualité terminée."),
  fix_script_label  = c(e = "Fix script:",                      f = "Script de corrections :"),
  fix_appended      = c(e = "Fix appended to",                  f = "Correction ajoutée à"),
  fix_exists        = c(e = "Fix script already exists:",       f = "Le script de corrections existe déjà :"),
  fix_removed       = c(e = "Existing fix script removed. Starting fresh.", f = "Script existant supprimé. Nouveau départ."),
  fix_created       = c(e = "Created fix accumulator:",         f = "Accumulateur de corrections créé :"),
  fix_header_title  = c(e = "QC FIXES",                         f = "CORRECTIONS QC"),
  fix_load_comment  = c(e = "# Load data before running fixes:\n# source(\"interactive_mission_qc_console.R\")\n",
                         f = "# Charger les données avant d'appliquer les corrections :\n# source(\"interactive_mission_qc_console.R\")\n"),
  keep_or_new       = c(e = "  [k]eep and append to existing / [n]ew clean script > ",
                         f = "  [k]onserver et ajouter / [n]ouveau script propre > "),
  mission_label     = c(e = "Mission:",                         f = "Mission :"),
  issue_label       = c(e = "Issue   :",                        f = "Problème :"),
  variable_label    = c(e = "Variable:",                        f = "Variable :"),
  miss_dist         = c(e = "MISSION DISTRIBUTION:",            f = "DISTRIBUTION DANS LA MISSION :"),
  missing_na        = c(e = "  missing (NA):",                  f = "  manquants (NA) :"),
  rows_missing      = c(e = "ROWS WITH MISSING VALUES (with context):", f = "LIGNES AVEC VALEURS MANQUANTES (avec contexte) :"),
  catalog_check     = c(e = "CATALOG CHECK:",                   f = "VÉRIFICATION DU CATALOGUE :"),
  did_you_mean      = c(e = "Did you mean:",                    f = "Vouliez-vous dire :"),
  suggested_fix     = c(e = "SUGGESTED FIX (R code):",          f = "CORRECTION SUGGÉRÉE (code R) :"),
  miss_num_dist     = c(e = "MISSION NUMERIC DISTRIBUTION:",    f = "DISTRIBUTION NUMÉRIQUE DANS LA MISSION :"),
  n_label           = c(e = "  n      :",                       f = "  n      :"),
  missing_label     = c(e = "  missing:",                       f = "  manquants :"),
  min_label         = c(e = "  min    :",                       f = "  min    :"),
  median_label      = c(e = "  median :",                       f = "  médiane :"),
  max_label         = c(e = "  max    :",                       f = "  max    :"),
  global_ctx        = c(e = "GLOBAL CONTEXT (numeric baselines):", f = "CONTEXTE GLOBAL (statistiques de référence) :"),
  outliers_label    = c(e = "OUTLIERS IN THIS MISSION:",        f = "VALEURS ABERRANTES DANS CETTE MISSION :"),
  n_outliers        = c(e = "  n outliers:",                    f = "  n aberrantes :"),
  min_outlier       = c(e = "  min outlier:",                   f = "  min aberrante :"),
  max_outlier       = c(e = "  max outlier:",                   f = "  max aberrante :"),
  outlier_rows      = c(e = "OUTLIER ROWS (with context):",     f = "LIGNES ABERRANTES (avec contexte) :"),
  qa_actions        = c(e = "SUGGESTED QA ACTIONS:",            f = "ACTIONS QA SUGGÉRÉES :"),
  qa_units          = c(e = "  • Verify units (global median:", f = "  • Vérifier les unités (médiane globale :"),
  qa_source         = c(e = "  • Inspect source records",       f = "  • Inspecter les données sources"),
  qa_cap            = c(e = "  • Decide if capping or leaving as-is is appropriate", f = "  • Décider si un plafonnement ou le maintien en l'état est approprié"),
  na_options        = c(e = "MISSING VALUE OPTIONS:",           f = "OPTIONS POUR LES VALEURS MANQUANTES :"),
  na_opt1           = c(e = "  [1] Leave as-is (keep NA)",      f = "  [1] Laisser tel quel (garder NA)"),
  na_opt2           = c(e = "  [2] Replace with mission mean",  f = "  [2] Remplacer par la moyenne de la mission"),
  na_opt3           = c(e = "  [3] Replace with mission median",f = "  [3] Remplacer par la médiane de la mission"),
  na_opt4           = c(e = "  [4] Enter a custom value",       f = "  [4] Entrer une valeur personnalisée"),
  na_skip           = c(e = "  [Enter] Skip / decide later",    f = "  [Entrée] Passer / décider plus tard"),
  na_prompt         = c(e = "Choose NA fix option [1/2/3/4/Enter]: ", f = "Choisir l'option pour les NA [1/2/3/4/Entrée] : "),
  na_custom_prompt  = c(e = "Enter value for",                  f = "Entrer une valeur pour"),
  generated_fix     = c(e = "GENERATED FIX:",                   f = "CORRECTION GÉNÉRÉE :"),
  template_label    = c(e = "OPTIONAL TEMPLATE (NOT APPLIED):", f = "MODÈLE OPTIONNEL (NON APPLIQUÉ) :"),
  bounds_label      = c(e = "MISSION BOUNDS (date-normalized):",f = "BORNES DE LA MISSION (date normalisée) :"),
  start_label       = c(e = "  start  :",                       f = "  début  :"),
  end_label         = c(e = "  end    :",                       f = "  fin    :"),
  offending_label   = c(e = "OFFENDING RECORDS:",               f = "ENREGISTREMENTS PROBLÉMATIQUES :"),
  count_label       = c(e = "  count   :",                      f = "  nombre  :"),
  earliest_label    = c(e = "  earliest:",                      f = "  plus tôt :"),
  latest_label      = c(e = "  latest  :",                      f = "  plus tard :"),
  fix_common        = c(e = "MOST COMMON FIX (row-level date + time):", f = "CORRECTION LA PLUS COURANTE (date + heure par ligne) :"),
  fix_alt           = c(e = "ALTERNATIVE (hour-only semantics):",f = "ALTERNATIVE (heure seulement) :"),
  no_baseline       = c(e = "No global numeric baseline available.", f = "Aucune statistique de référence globale disponible."),
  no_date_col       = c(e = "No date column found.",             f = "Aucune colonne de date trouvée."),
  dates_available   = c(e = "Available dates:",                  f = "Dates disponibles :"),
  date_prompt       = c(e = "Enter date number (or Enter to skip): ", f = "Entrer le numéro de date (ou Entrée pour passer) : "),
  all_rows_day      = c(e = "All rows for",                      f = "Toutes les lignes pour"),
  rows_label        = c(e = "rows",                              f = "lignes"),
  opt_view          = c(e = "Options: [v]iew details / [s]kip / [n]ext mission / [q]uit > ",
                         f = "Options : [v]oir détails / [s]auter / [n]ext mission / [q]uitter > "),
  opt_post          = c(e = "Options: [d]ay inspect / [a]dd fix to script / [Enter] to continue > ",
                         f = "Options : [d]ate inspecter / [a]jouter correction / [Entrée] pour continuer > "),
  opt_post_no_fix   = c(e = "Options: [d]ay inspect / [Enter] to continue > ",
                         f = "Options : [d]ate inspecter / [Entrée] pour continuer > "),
  # Code comments in generated fixes
  code_reviewer     = c(e = "# reviewer decision",              f = "# décision du réviseur"),
  code_leave_na     = c(e = "# Leave",                          f = "# Laisser"),
  code_no_change    = c(e = "# no change needed",               f = "# aucun changement nécessaire")
)

msg <- function(key) .strings[[key]][[.lang]]

# ============================================================
# CONFIG
# ============================================================

# Always resolve paths relative to this script's location (repo root),
# regardless of the working directory on any platform.
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

# Folder structure (identical on all platforms — everything is in the repo):
#   SOMEC_Editeur/               <- THIS repo (.repo_root)
#   ├── GlobalContext/
#   │   ├── _cache/              <- missions/transects/observations.rds
#   │   └── global_context.rds
#   ├── MissionReports/
#   │   └── mission_issues.rds
#   ├── RelCatalog.xlsx
#   └── interactive_mission_qc_console.R
#
# On Windows only: .accdb is outside the repo at:
#   SOMEC/GestionDeDonnees/BaseDeDonnees/SOMEC_YYYYMMDD.accdb

cfg <- list(
  accdb_path          = if (.Platform$OS.type == "windows")
                          file.path(dirname(dirname(.repo_root)), "BaseDeDonnees", "SOMEC_20251106.accdb")
                        else
                          NULL,
  context_dir         = file.path(.repo_root, "GlobalContext"),
  mission_reports_dir = file.path(.repo_root, "MissionReports"),
  cache_dir           = file.path(.repo_root, "GlobalContext", "_cache")
)

dir.create(cfg$cache_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# HELPERS
# ============================================================

# Derive the accdb version tag from the filename (e.g. "SOMEC_20251106")
.accdb_version <- if (!is.null(cfg$accdb_path)) {
  tools::file_path_sans_ext(basename(cfg$accdb_path))
} else {
  paste0("SOMEC_", format(Sys.Date(), "%Y%m%d"))
}

.fixes_script <- file.path(.repo_root, paste0("apply_qc_fixes_", .accdb_version, ".R"))

# Track whether the fixes script has been initialised this session
.fixes_initialised <- FALSE

# Append a fix snippet to the accumulated fixes script
append_fix <- function(snippet, mission_id, issue_type, tbl, col) {

  if (!.fixes_initialised) {
    if (file.exists(.fixes_script)) {
      cat("\n📄 ", msg("fix_exists"), basename(.fixes_script), "\n")
      keep <- prompt(msg("keep_or_new"), c("k", "n"))
      if (keep == "n") {
        file.remove(.fixes_script)
        cat("🗑️  ", msg("fix_removed"), "\n")
      }
    }
    cat(
      "# ============================================================\n",
      "# ", msg("fix_header_title"), " — ", .accdb_version, "\n",
      "# Generated by interactive_mission_qc_console.R\n",
      "# ============================================================\n\n",
      msg("fix_load_comment"), "\n",
      file = .fixes_script, sep = "", append = file.exists(.fixes_script)
    )
    .fixes_initialised <<- TRUE
  }

  header <- paste0(
    "\n# ── ", mission_id, " | ", issue_type, " | ", tbl, "$", col,
    " [", format(Sys.time(), "%Y-%m-%d %H:%M"), "] ──\n"
  )
  cat(header, snippet, "\n", file = .fixes_script, append = TRUE)
  cat("✅ ", msg("fix_appended"), basename(.fixes_script), "\n")
}

prompt <- function(msg_txt, valid = NULL) {
  repeat {
    ans <- readline(msg_txt)
    ans <- trimws(ans)
    if (is.null(valid) || ans %in% valid) return(ans)
    cat("Invalid choice. Try again.\n")
  }
}

print_dist <- function(x, top = 10) {
  tibble(value = as.character(x)) |>
    filter(!is.na(value)) |>
    count(value, sort = TRUE) |>
    mutate(pct = round(100 * n / sum(n), 1)) |>
    slice_head(n = top) |>
    print(n = Inf)
}

generate_recode_snippet <- function(table, col, map) {
  rules <- paste0(
    "      ", col, " == \"", names(map), "\" ~ \"", map, "\"",
    collapse = ",\n"
  )
  paste0(
    table, " <- ", table, " |>\n",
    "  mutate(\n",
    "    ", col, " = case_when(\n",
    rules, ",\n",
    "      TRUE ~ ", col, "\n",
    "    )\n",
    "  )"
  )
}

get_mission_bounds <- function(missions, mission_id) {
  row <- missions |> filter(mission == mission_id) |> slice_head(n = 1)
  list(
    start = suppressWarnings(as.POSIXct(row$debut)),
    end   = suppressWarnings(as.POSIXct(row$fin))
  )
}

get_global_numeric_baseline <- function(global_ctx, table, column) {
  global_ctx$numeric_baselines |>
    filter(table == !!table, column == !!column) |>
    slice_head(n = 1)
}

load_access_table_cached <- function(accdb_path, table_name, cache_dir) {
  cache_file <- file.path(cache_dir, paste0(table_name, ".rds"))

  cache_exists <- file.exists(cache_file)
  accdb_exists <- !is.null(accdb_path) && file.exists(accdb_path)

  if (cache_exists && accdb_exists) {
    cache_mtime <- file.info(cache_file)$mtime
    accdb_mtime <- file.info(accdb_path)$mtime
    if (!is.na(cache_mtime) && !is.na(accdb_mtime) && cache_mtime >= accdb_mtime) {
      return(readRDS(cache_file))
    }
  }

  if (cache_exists && !accdb_exists) {
    message("⚠️  '", basename(accdb_path %||% "accdb"), "' ", msg("no_accdb"),
            " ", msg("loading_cache"), " '", table_name, "'.")
    return(readRDS(cache_file))
  }

  if (!accdb_exists) {
    stop(
      "No cache found for '", table_name, "' and the .accdb is not accessible.\n",
      "Expected .accdb at: ", accdb_path, "\n",
      "Run the script on Windows to generate the .rds cache files."
    )
  }

  if (!exists("odbcConnectAccess2007", where = asNamespace("RODBC"), inherits = FALSE)) {
    stop("odbcConnectAccess2007 is not available on this platform (macOS).")
  }
  con <- RODBC::odbcConnectAccess2007(accdb_path, believeNRows = FALSE)
  on.exit(RODBC::odbcClose(con), add = TRUE)
  df <- RODBC::sqlFetch(con, table_name) |> clean_names()
  saveRDS(df, cache_file)
  message("✅  Cached '", table_name, "' to ", cache_file)
  df
}

# Reorder columns so the variable of interest appears first, followed by key identifiers
show_rows <- function(df, col) {
  id_cols    <- intersect(c("mission", "id_transect", "id", "date", "heure", "site"), names(df))
  id_cols    <- setdiff(id_cols, col)
  other_cols <- setdiff(names(df), c(col, id_cols))
  df |>
    select(all_of(c(col, id_cols, other_cols))) |>
    as_tibble() |>
    print(n = Inf)
}

# Show flagged rows with n_context rows before and after each flagged row for context
show_rows_with_context <- function(df, col, flag_idx, n_context = 2) {
  n <- nrow(df)
  context_idx <- unique(unlist(lapply(flag_idx, function(i) {
    max(1, i - n_context):min(n, i + n_context)
  })))
  context_idx <- sort(context_idx)

  id_cols    <- intersect(c("mission", "id_transect", "id", "date", "heure", "site"), names(df))
  id_cols    <- setdiff(id_cols, col)
  other_cols <- setdiff(names(df), c(col, id_cols))

  df_show <- df[context_idx, ] |>
    select(all_of(c(col, id_cols, other_cols))) |>
    as_tibble() |>
    mutate(.flag = if_else(row_number() %in% which(context_idx %in% flag_idx), "<<", ""),
           .before = 1)

  print(df_show, n = Inf)
}

# Show all rows for a selected date
show_rows_for_day <- function(df, col, date_col = "date") {
  if (!date_col %in% names(df)) {
    cat("⚠️ ", msg("no_date_col"), "\n")
    return(invisible(NULL))
  }
  dates_available <- sort(unique(as.Date(df[[date_col]])))
  cat(msg("dates_available"), "\n")
  for (i in seq_along(dates_available)) {
    cat(sprintf("  [%d] %s\n", i, dates_available[i]))
  }
  choice <- readline(msg("date_prompt"))
  choice <- suppressWarnings(as.integer(trimws(choice)))
  if (is.na(choice) || choice < 1 || choice > length(dates_available)) {
    return(invisible(NULL))
  }
  selected_date <- dates_available[choice]
  day_rows <- df[as.Date(df[[date_col]]) == selected_date, ]
  cat(sprintf("\n%s %s (%d %s):\n",
      msg("all_rows_day"), selected_date, nrow(day_rows), msg("rows_label")))
  show_rows(day_rows, col)
}

generate_recode_snippet <- function(table, col, map) {
  rules <- paste0(
    "      ", col, " == \"", names(map), "\" ~ \"", map, "\"",
    collapse = ",\n"
  )
  paste0(
    table, " <- ", table, " |>\n",
    "  mutate(\n",
    "    ", col, " = case_when(\n",
    rules, ",\n",
    "      TRUE ~ ", col, "\n",
    "    )\n",
    "  )"
  )
}

get_mission_bounds <- function(missions, mission_id) {
  row <- missions |> filter(mission == mission_id) |> slice_head(n = 1)
  list(
    start = suppressWarnings(as.POSIXct(row$debut)),
    end   = suppressWarnings(as.POSIXct(row$fin))
  )
}

get_global_numeric_baseline <- function(global_ctx, table, column) {
  global_ctx$numeric_baselines |>
    filter(table == !!table, column == !!column) |>
    slice_head(n = 1)
}

load_access_table_cached <- function(accdb_path, table_name, cache_dir) {
  cache_file <- file.path(cache_dir, paste0(table_name, ".rds"))

  cache_exists <- file.exists(cache_file)
  accdb_exists <- !is.null(accdb_path) && file.exists(accdb_path)

  # Use cache if it exists and is at least as recent as the .accdb
  if (cache_exists && accdb_exists) {
    cache_mtime <- file.info(cache_file)$mtime
    accdb_mtime <- file.info(accdb_path)$mtime
    if (!is.na(cache_mtime) && !is.na(accdb_mtime) && cache_mtime >= accdb_mtime) {
      return(readRDS(cache_file))
    }
  }

  # Use cache if .accdb is not accessible (macOS or path wrong)
  if (cache_exists && !accdb_exists) {
    message("⚠️  '", basename(accdb_path %||% "accdb"), "' not found. Loading cached version of '", table_name, "'.")
    return(readRDS(cache_file))
  }

  # No cache and no .accdb — cannot proceed
  if (!accdb_exists) {
    stop(
      "No cache found for '", table_name, "' and the .accdb is not accessible.\n",
      "Expected .accdb at: ", accdb_path, "\n",
      "Run the script on Windows to generate the .rds cache files."
    )
  }

  # .accdb is accessible — read and cache
  if (!exists("odbcConnectAccess2007", where = asNamespace("RODBC"), inherits = FALSE)) {
    stop("odbcConnectAccess2007 is not available on this platform (macOS).")
  }
  con <- RODBC::odbcConnectAccess2007(accdb_path, believeNRows = FALSE)
  on.exit(RODBC::odbcClose(con), add = TRUE)
  df <- RODBC::sqlFetch(con, table_name) |> clean_names()
  saveRDS(df, cache_file)
  message("✅  Cached '", table_name, "' to ", cache_file)
  df
}

# ============================================================
# LOAD DATA
# ============================================================

missions     <- load_access_table_cached(cfg$accdb_path, "missions",     cfg$cache_dir) |> clean_names()
transects    <- load_access_table_cached(cfg$accdb_path, "transects",    cfg$cache_dir) |> clean_names()
observations <- load_access_table_cached(cfg$accdb_path, "observations", cfg$cache_dir) |> clean_names()

# ============================================================
# LOAD GLOBAL CONTEXT
# ============================================================

global_ctx  <- readRDS(file.path(cfg$context_dir, "global_context.rds"))
catalog_map <- global_ctx$catalog_map

# ============================================================
# LOAD MISSION ISSUES
# ============================================================

mission_issues <- readRDS(file.path(cfg$mission_reports_dir, "mission_issues.rds"))

# ============================================================
# ISSUE EXPLANATION  (returns fix snippet invisibly)
# ============================================================

explain_issue <- function(issue_row, mission_id) {

  tbl     <- issue_row$table
  col     <- issue_row$column
  df_mis  <- get(tbl) |> filter(mission == mission_id)
  snippet <- NULL

  cat("\n--------------------------------------------\n")
  cat(msg("mission_label"), mission_id, "\n")
  cat(msg("issue_label"),   issue_row$issue_type, "\n")
  cat(msg("variable_label"), paste0(tbl, "$", col), "\n")
  cat("--------------------------------------------\n\n")

  # ============================================================
  # CATEGORICAL — UNKNOWN CATALOG
  # ============================================================

  if (issue_row$issue_type == "UNKNOWN_CATALOG") {

    cat(msg("miss_dist"), "\n")
    print_dist(df_mis[[col]])

    n_na <- sum(is.na(df_mis[[col]]))
    cat(msg("missing_na"), n_na, "\n\n")

    if (n_na > 0) {
      cat(msg("rows_missing"), "\n")
      na_idx <- which(is.na(df_mis[[col]]))
      show_rows_with_context(df_mis, col, na_idx)
      cat("\n")
    }

    allowed  <- catalog_map[[paste0(tolower(tbl), "$", tolower(col))]]
    unknowns <- strsplit(issue_row$details, ",\\s*")[[1]]

    recode_map <- character()

    cat("\n", msg("catalog_check"), "\n")
    for (u in unknowns) {
      cat(" ❌", u, "\n")
      d    <- stringdist(u, allowed, method = "lv")
      best <- allowed[which.min(d)]
      cat("    💡", msg("did_you_mean"), best, "\n")
      recode_map[u] <- best
    }

    if (length(recode_map)) {
      snippet <- generate_recode_snippet(tbl, col, recode_map)
      cat("\n", msg("suggested_fix"), "\n\n")
      cat(snippet, "\n")
    }
    return(invisible(snippet))
  }

  # ============================================================
  # NUMERIC — OUTLIERS USING GLOBAL CONTEXT
  # ============================================================

  if (issue_row$issue_type == "NUMERIC_OUTLIER") {

    x    <- suppressWarnings(as.numeric(df_mis[[col]]))
    n_na <- sum(is.na(x))

    cat(msg("miss_num_dist"), "\n")
    cat(msg("n_label"),      sum(!is.na(x)), "\n")
    cat(msg("missing_label"), n_na, "\n")
    cat(msg("min_label"),    min(x, na.rm = TRUE), "\n")
    cat(msg("median_label"), median(x, na.rm = TRUE), "\n")
    cat(msg("max_label"),    max(x, na.rm = TRUE), "\n\n")

    if (n_na > 0) {
      cat(msg("rows_missing"), "\n")
      na_idx <- which(is.na(x))
      show_rows_with_context(df_mis, col, na_idx)
      cat("\n")
    }

    gb <- get_global_numeric_baseline(global_ctx, tbl, col)

    if (nrow(gb) == 0) {
      cat("⚠️ ", msg("no_baseline"), "\n")
      return(invisible(NULL))
    }

    cat(msg("global_ctx"), "\n")
    cat("  p05 :", gb$p05, "\n")
    cat("  p50 :", gb$p50, "\n")
    cat("  p95 :", gb$p95, "\n\n")

    outliers <- x < gb$p05 | x > gb$p95

    cat(msg("outliers_label"), "\n")
    cat(msg("n_outliers"), sum(outliers, na.rm = TRUE), "\n")

    if (any(outliers, na.rm = TRUE)) {
      cat(msg("min_outlier"), min(x[outliers], na.rm = TRUE), "\n")
      cat(msg("max_outlier"), max(x[outliers], na.rm = TRUE), "\n\n")

      cat(msg("outlier_rows"), "\n")
      outlier_idx <- which(!is.na(outliers) & outliers)
      show_rows_with_context(df_mis, col, outlier_idx)

      snippet <- paste0(
        tbl, " <- ", tbl, " |>\n",
        "  mutate(\n",
        "    ", col, " = ", col, "  ", msg("code_reviewer"), "\n",
        "  )"
      )
    }

    # Plot — shown in Positron viewer pane
    plot_df <- tibble(value = ifelse(is.na(x), gb$p50, x),
                      category = case_when(
                        is.na(x)                    ~ if (.lang == "f") "Manquant (NA)" else "Missing (NA)",
                        coalesce(outliers, FALSE)    ~ if (.lang == "f") "Aberrant"      else "Outlier",
                        TRUE                         ~ if (.lang == "f") "Normal"        else "Normal"
                      ))

    p <- ggplot(plot_df, aes(x = 0, y = value)) +
      geom_jitter(aes(color = category, shape = category), width = 0.15, size = 2, alpha = 0.7) +
      geom_hline(yintercept = gb$p05, linetype = "dashed", color = "steelblue", linewidth = 0.8) +
      geom_hline(yintercept = gb$p95, linetype = "dashed", color = "steelblue", linewidth = 0.8) +
      geom_hline(yintercept = gb$p50, linetype = "dotted", color = "grey40",    linewidth = 0.6) +
      scale_color_manual(values = c(
        "Normal" = "grey60", "Outlier" = "firebrick", "Missing (NA)" = "orange",
        "Normal" = "grey60", "Aberrant" = "firebrick", "Manquant (NA)" = "orange"
      )) +
      scale_shape_manual(values = c(
        "Normal" = 16, "Outlier" = 16, "Missing (NA)" = 4,
        "Normal" = 16, "Aberrant" = 16, "Manquant (NA)" = 4
      )) +
      labs(
        title    = paste0(if (.lang == "f") "QC Numérique — " else "Numeric QC — ", tbl, "$", col),
        subtitle = paste0(if (.lang == "f") "Mission : " else "Mission: ", mission_id,
                          "  |  p05=", gb$p05, "  p50=", gb$p50, "  p95=", gb$p95,
                          "  |  NA=", n_na),
        y = col, x = NULL, color = NULL, shape = NULL
      ) +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

    .plot_file <- file.path(tempdir(), "qc_numeric_plot.png")
    ggsave(.plot_file, plot = p, width = 4, height = 6, dpi = 120)
    rstudioapi::viewer(.plot_file)

    cat("\n", msg("qa_actions"), "\n")
    cat(msg("qa_units"), gb$p50, ")\n")
    cat(msg("qa_source"), "\n")
    cat(msg("qa_cap"), "\n\n")

    # Interactive NA fix menu
    if (n_na > 0) {
      mis_mean   <- round(mean(x, na.rm = TRUE), 4)
      mis_median <- round(median(x, na.rm = TRUE), 4)

      cat(msg("na_options"), "\n")
      cat(msg("na_opt1"), "\n")
      cat(msg("na_opt2"), " (", mis_mean, ")\n")
      cat(msg("na_opt3"), " (", mis_median, ")\n")
      cat(msg("na_opt4"), "\n")
      cat(msg("na_skip"), "\n\n")

      na_choice <- prompt(msg("na_prompt"), c("1", "2", "3", "4", ""))

      snippet <- if (na_choice == "1") {
        paste0(msg("code_leave_na"), " ", col, " NA — ", msg("code_no_change"), "\n")
      } else if (na_choice == "2") {
        paste0(
          tbl, " <- ", tbl, " |>\n",
          "  mutate(\n",
          "    ", col, " = if_else(mission == \"", mission_id, "\" & is.na(", col, "),\n",
          "      ", mis_mean, ", ", col, ")\n",
          "  )"
        )
      } else if (na_choice == "3") {
        paste0(
          tbl, " <- ", tbl, " |>\n",
          "  mutate(\n",
          "    ", col, " = if_else(mission == \"", mission_id, "\" & is.na(", col, "),\n",
          "      ", mis_median, ", ", col, ")\n",
          "  )"
        )
      } else if (na_choice == "4") {
        custom <- readline(paste0(msg("na_custom_prompt"), " ", col, ": "))
        custom <- trimws(custom)
        paste0(
          tbl, " <- ", tbl, " |>\n",
          "  mutate(\n",
          "    ", col, " = if_else(mission == \"", mission_id, "\" & is.na(", col, "),\n",
          "      ", custom, ", ", col, ")\n",
          "  )"
        )
      } else {
        NULL  # skip
      }

      if (!is.null(snippet)) {
        cat("\n", msg("generated_fix"), "\n\n")
        cat(snippet, "\n")
      }
    } else if (!is.null(snippet)) {
      cat(msg("template_label"), "\n\n")
      cat(snippet, "\n")
    }

    return(invisible(snippet))
  }
  
  # ============================================================
  # DATETIME — OUTSIDE MISSION DATES
  # ============================================================
  
  if (issue_row$issue_type == "OUTSIDE_MISSION_DATES") {

    raw_bounds <- get_mission_bounds(missions, mission_id)

    mis_start <- as.POSIXct(
      paste(as.Date(raw_bounds$start), "00:00:00"), tz = "UTC"
    )
    mis_end <- as.POSIXct(
      paste(as.Date(raw_bounds$end), "23:59:59"), tz = "UTC"
    )

    outside <- df_mis |>
      filter(
        !is.na(.data[[col]]) &
          (.data[[col]] < mis_start | .data[[col]] > mis_end)
      )

    n_na <- sum(is.na(df_mis[[col]]))

    cat(msg("bounds_label"), "\n")
    cat(msg("start_label"), format(mis_start, "%Y-%m-%d %H:%M:%S"), "\n")
    cat(msg("end_label"),   format(mis_end,   "%Y-%m-%d %H:%M:%S"), "\n")
    cat(msg("missing_label"), n_na, "\n\n")

    if (n_na > 0) {
      cat(msg("rows_missing"), "\n")
      na_idx <- which(is.na(df_mis[[col]]))
      show_rows_with_context(df_mis, col, na_idx)
      cat("\n")
    }

    dt_vals <- as.POSIXct(outside[[col]], origin = "1970-01-01", tz = "UTC")

    cat(msg("offending_label"), "\n")
    cat(msg("count_label"),   length(dt_vals), "\n")
    if (length(dt_vals) > 0) {
      cat(msg("earliest_label"), format(min(dt_vals), "%Y-%m-%d %H:%M:%S"), "\n")
      cat(msg("latest_label"),   format(max(dt_vals), "%Y-%m-%d %H:%M:%S"), "\n")
    }
    
    snippet <- paste0(
      tbl, " <- ", tbl, " |>\n",
      "  mutate(\n",
      "    ", col, " = if_else(\n",
      "      mission == \"", mission_id, "\" &\n",
      "      (", col, " < as.POSIXct(\"",
      format(mis_start, "%Y-%m-%d %H:%M:%S"), "\") |\n",
      "       ", col, " > as.POSIXct(\"",
      format(mis_end, "%Y-%m-%d %H:%M:%S"), "\")),\n",
      "      as.POSIXct(paste(as.Date(date), format(", col, ", \"%H:%M:%S\")), tz=\"UTC\"),\n",
      "      ", col, "\n",
      "    )\n",
      "  )"
    )

    cat("\n✅", msg("fix_common"), "\n\n")
    cat(snippet, "\n")

    cat("\n", msg("fix_alt"), "\n\n")
    cat(
      tbl, " <- ", tbl, " |>\n",
      "  mutate(\n",
      "    ", col, " = as.POSIXct(paste(\"1970-01-01\", format(", col, ", \"%H:%M:%S\")), tz=\"UTC\")\n",
      "  )\n",
      sep = ""
    )

    return(invisible(snippet))
  }
}

# ============================================================
# INTERACTIVE LOOP
# ============================================================

run_interactive_qc <- function() {

  for (mis in unique(mission_issues$mission)) {

    cat("\n================================================\n")
    cat(msg("mission_label"), mis, "\n")
    cat("================================================\n")

    issues <- mission_issues |> filter(mission == mis)

    for (j in seq_len(nrow(issues))) {

      issue <- issues[j, ]

      # Count missing values for this variable in this mission
      df_mis <- get(issue$table) |> filter(mission == mis)
      n_na   <- sum(is.na(df_mis[[issue$column]]))
      na_tag <- if (n_na > 0) paste0("  [", n_na, " NA]") else ""

      cat(sprintf(
        "\n[%d] %s — %s$%s%s\n",
        j, issue$issue_type, issue$table, issue$column, na_tag
      ))

      choice <- prompt(msg("opt_view"), c("v", "s", "n", "q"))

      if (choice == "v") {
        snippet <- explain_issue(issue, mis)
        cat("\n")

        df_mis <- get(issue$table) |> filter(mission == mis)

        repeat {
          menu_msg  <- if (!is.null(snippet)) msg("opt_post") else msg("opt_post_no_fix")
          valid_opt <- if (!is.null(snippet)) c("d", "a", "") else c("d", "")

          post <- prompt(menu_msg, valid_opt)

          if (post == "d") {
            show_rows_for_day(df_mis, issue$column)
          } else if (post == "a") {
            append_fix(snippet, mis, issue$issue_type, issue$table, issue$column)
            break
          } else {
            break
          }
        }
      }

      if (choice == "n") break
      if (choice == "q") return(invisible(NULL))
    }
  }

  cat("\n✅ ", msg("session_complete"), "\n")
  if (file.exists(.fixes_script)) {
    cat("📄 ", msg("fix_script_label"), .fixes_script, "\n")
  }
}


# ============================================================
# RUN
# ============================================================

run_interactive_qc()
