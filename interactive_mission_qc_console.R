suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(RODBC)
  library(stringdist)
  library(base64enc)
})

# ============================================================
# ENVIRONMENT CHECK
# ============================================================

# readline() does not work when a script is sourced in RStudio.
# This script must be run line-by-line (Ctrl+Enter) or via
# Positron where readline() is fully interactive.
.in_rstudio <- identical(Sys.getenv("RSTUDIO"), "1")
.is_sourced <- sys.nframe() > 0

if (.in_rstudio && .is_sourced) {
  stop(
    "\n\nThis script cannot be sourced in RStudio because readline() does not\n",
    "accept input during source(). Please either:\n",
    "  1. Run it line-by-line in RStudio (select all + Ctrl+Enter), or\n",
    "  2. Use Positron, where source() and readline() work together.\n",
    call. = FALSE
  )
}

# ============================================================
# LANGUAGE
# ============================================================
# R
select_language <- function() {
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::hasFun("showPrompt")) {
    ans <- tryCatch(rstudioapi::showPrompt("Language", "Language / Langue: [e] English  [f] Français", default = "e"),
                    error = function(e) NA_character_)
    .lang <<- if (!is.na(ans)) tolower(trimws(ans)) else "e"
  } else if (interactive()) {
    cat("Language / Langue: [e] English  [f] Français > ")
    .lang <<- tolower(trimws(readline()))
  } else {
    .lang <<- "e"
  }
  if (!.lang %in% c("e","f")) .lang <<- "e"
}
select_language()

.strings <- list(
  loading_cache    = c(e = "Loading cached version of",       f = "Chargement de la version en cache de"),
  no_accdb         = c(e = "not found.",                      f = "introuvable."),
  session_complete = c(e = "QC session complete.",            f = "Session de contrôle qualité terminée."),
  fix_script_label = c(e = "Fix script:",                     f = "Script de corrections :"),
  fix_appended     = c(e = "Fix appended to",                 f = "Correction ajoutée à"),
  fix_exists       = c(e = "Fix script already exists:",      f = "Le script de corrections existe déjà :"),
  fix_removed      = c(e = "Existing fix script removed. Starting fresh.", f = "Script existant supprimé. Nouveau départ."),
  fix_header_title = c(e = "QC FIXES",                        f = "CORRECTIONS QC"),
  fix_load_comment = c(e = "# Load data before running fixes:\n# source(\"interactive_mission_qc_console.R\")\n",
                        f = "# Charger les données avant d'appliquer les corrections :\n# source(\"interactive_mission_qc_console.R\")\n"),
  keep_or_new      = c(e = "  [k]eep and append to existing / [n]ew clean script > ",
                        f = "  [k]onserver et ajouter / [n]ouveau script propre > "),
  mission_label    = c(e = "Mission:",                        f = "Mission :"),
  issue_label      = c(e = "Issue   :",                       f = "Problème :"),
  variable_label   = c(e = "Variable:",                       f = "Variable :"),
  miss_dist        = c(e = "MISSION DISTRIBUTION:",           f = "DISTRIBUTION DANS LA MISSION :"),
  missing_na       = c(e = "  missing (NA):",                 f = "  manquants (NA) :"),
  rows_missing     = c(e = "ROWS WITH MISSING VALUES:",       f = "LIGNES AVEC VALEURS MANQUANTES :"),
  catalog_check    = c(e = "CATALOG CHECK:",                  f = "VÉRIFICATION DU CATALOGUE :"),
  did_you_mean     = c(e = "Did you mean:",                   f = "Vouliez-vous dire :"),
  suggested_fix    = c(e = "SUGGESTED FIX (R code):",         f = "CORRECTION SUGGÉRÉE (code R) :"),
  miss_num_dist    = c(e = "MISSION NUMERIC DISTRIBUTION:",   f = "DISTRIBUTION NUMÉRIQUE DANS LA MISSION :"),
  n_label          = c(e = "  n      :",                      f = "  n      :"),
  missing_label    = c(e = "  missing:",                      f = "  manquants :"),
  min_label        = c(e = "  min    :",                      f = "  min    :"),
  median_label     = c(e = "  median :",                      f = "  médiane :"),
  max_label        = c(e = "  max    :",                      f = "  max    :"),
  global_ctx       = c(e = "GLOBAL CONTEXT (numeric baselines):", f = "CONTEXTE GLOBAL (statistiques de référence) :"),
  outliers_label   = c(e = "OUTLIERS IN THIS MISSION:",       f = "VALEURS ABERRANTES DANS CETTE MISSION :"),
  n_outliers       = c(e = "  n outliers:",                   f = "  n aberrantes :"),
  min_outlier      = c(e = "  min outlier:",                  f = "  min aberrante :"),
  max_outlier      = c(e = "  max outlier:",                  f = "  max aberrante :"),
  outlier_rows     = c(e = "OUTLIER ROWS (with context):",    f = "LIGNES ABERRANTES (avec contexte) :"),
  qa_actions       = c(e = "SUGGESTED QA ACTIONS:",           f = "ACTIONS QA SUGGÉRÉES :"),
  qa_units         = c(e = "  • Verify units (global median:", f = "  • Vérifier les unités (médiane globale :"),
  qa_source        = c(e = "  • Inspect source records",      f = "  • Inspecter les données sources"),
  qa_cap           = c(e = "  • Decide if capping or leaving as-is is appropriate", f = "  • Décider si un plafonnement ou le maintien en l'état est approprié"),
  na_options       = c(e = "MISSING VALUE OPTIONS:",          f = "OPTIONS POUR LES VALEURS MANQUANTES :"),
  na_opt1          = c(e = "  [1] Leave as-is (keep NA)",     f = "  [1] Laisser tel quel (garder NA)"),
  na_opt2          = c(e = "  [2] Replace with mission mean", f = "  [2] Remplacer par la moyenne de la mission"),
  na_opt3          = c(e = "  [3] Replace with mission median", f = "  [3] Remplacer par la médiane de la mission"),
  na_opt4          = c(e = "  [4] Enter a custom value",      f = "  [4] Entrer une valeur personnalisée"),
  na_skip          = c(e = "  [Enter] Skip / decide later",   f = "  [Entrée] Passer / décider plus tard"),
  na_prompt        = c(e = "Choose NA fix option [1/2/3/4/Enter]: ", f = "Choisir l'option pour les NA [1/2/3/4/Entrée] : "),
  na_custom_prompt = c(e = "Enter value for",                 f = "Entrer une valeur pour"),
  generated_fix    = c(e = "GENERATED FIX:",                  f = "CORRECTION GÉNÉRÉE :"),
  template_label   = c(e = "OPTIONAL TEMPLATE (NOT APPLIED):", f = "MODÈLE OPTIONNEL (NON APPLIQUÉ) :"),
  bounds_label     = c(e = "MISSION BOUNDS (date-normalized):", f = "BORNES DE LA MISSION (date normalisée) :"),
  start_label      = c(e = "  start  :",                      f = "  début  :"),
  end_label        = c(e = "  end    :",                       f = "  fin    :"),
  offending_label  = c(e = "OFFENDING RECORDS:",              f = "ENREGISTREMENTS PROBLÉMATIQUES :"),
  count_label      = c(e = "  count   :",                     f = "  nombre  :"),
  earliest_label   = c(e = "  earliest:",                     f = "  plus tôt :"),
  latest_label     = c(e = "  latest  :",                     f = "  plus tard :"),
  fix_common       = c(e = "MOST COMMON FIX (row-level date + time):", f = "CORRECTION LA PLUS COURANTE (date + heure par ligne) :"),
  no_baseline      = c(e = "No global numeric baseline available.", f = "Aucune statistique de référence globale disponible."),
  no_date_col      = c(e = "No date column found.",           f = "Aucune colonne de date trouvée."),
  dates_available  = c(e = "Available dates:",                f = "Dates disponibles :"),
  date_prompt      = c(e = "Enter date number (or Enter to skip): ", f = "Entrer le numéro de date (ou Entrée pour passer) : "),
  all_rows_day     = c(e = "All rows for",                    f = "Toutes les lignes pour"),
  rows_label       = c(e = "rows",                            f = "lignes"),
  opt_view         = c(e = "Options: [v]iew details / [s]kip / [n]ext mission / [q]uit > ",
                        f = "Options : [v]oir détails / [s]auter / [n]ext mission / [q]uitter > "),
  code_reviewer    = c(e = "# reviewer decision",             f = "# décision du réviseur"),
  code_leave_na    = c(e = "# Leave",                         f = "# Laisser"),
  code_no_change   = c(e = "# no change needed",              f = "# aucun changement nécessaire")
)
msg <- function(key) {
  entry <- .strings[[key]]
  if (is.null(entry)) stop("Unknown i18n key: ", key)
  lang <- tryCatch(get(".lang", envir = .GlobalEnv), error = function(e) "e")
  if (!lang %in% c("e", "f")) lang <- "e"
  entry[[lang]]
}

# ============================================================
# CONFIG
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

resolve_accdb <- function(repo_root, suffix = NULL, accdb_path = NULL) {
  if (.Platform$OS.type != "windows") {
    return(list(version = NA_character_, path = NULL, db_dir = NULL))
  }

  # repo_root = .../SOMEC/BaseDeDonnees/GestionDeDonnees/SOMEC_Editeur
  # .accdb files live directly in .../SOMEC/BaseDeDonnees/ (2 levels up)
  db_dir <- normalizePath(
    dirname(dirname(repo_root)),
    winslash = "/", mustWork = FALSE
  )

  if (!is.null(accdb_path) && nzchar(accdb_path)) {
    p <- normalizePath(accdb_path, winslash = "/", mustWork = FALSE)
    return(list(
      version = tools::file_path_sans_ext(basename(p)),
      path = p,
      db_dir = db_dir
    ))
  }

  if (!is.null(suffix) && nzchar(suffix)) {
    v <- if (grepl("^SOMEC_", suffix)) suffix else paste0("SOMEC_", suffix)
    p <- normalizePath(file.path(db_dir, paste0(v, ".accdb")), winslash = "/", mustWork = FALSE)
    return(list(version = v, path = p, db_dir = db_dir))
  }

  files <- list.files(db_dir, pattern = "^SOMEC_\\d{8}\\.accdb$", full.names = TRUE)
  if (!length(files)) {
    return(list(version = NA_character_, path = NULL, db_dir = db_dir))
  }

  latest <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
  latest <- normalizePath(latest, winslash = "/", mustWork = FALSE)

  list(
    version = tools::file_path_sans_ext(basename(latest)),
    path = latest,
    db_dir = db_dir
  )
}

ask_accdb_choice <- function(repo_root) {
  env_path <- Sys.getenv("SOMEC_ACCDB_PATH", unset = "")
  env_suffix <- Sys.getenv("SOMEC_ACCDB_SUFFIX", unset = "")
  default <- resolve_accdb(repo_root, suffix = env_suffix, accdb_path = env_path)

  if (!interactive()) return(default)

  default_suffix <- if (is.na(default$version)) "auto" else sub("^SOMEC_", "", default$version)
  prompt_txt <- if (.lang == "f") {
    paste0("Suffixe ACCDB (YYYYMMDD) [Entrée = ", default_suffix, "] : ")
  } else {
    paste0("ACCDB suffix (YYYYMMDD) [Enter = ", default_suffix, "]: ")
  }

  ans <- trimws(readline(prompt_txt))
  if (nzchar(ans)) {
    return(resolve_accdb(repo_root, suffix = ans))
  }

  default
}

.accdb <- ask_accdb_choice(.repo_root)

cfg <- list(
  accdb_version       = .accdb$version,
  accdb_path          = .accdb$path,
  context_dir         = file.path(.repo_root, "GlobalContext"),
  mission_reports_dir = file.path(.repo_root, "MissionReports"),
  cache_dir           = file.path(.repo_root, "GlobalContext", "_cache")
)

if (is.null(cfg$accdb_path) || !nzchar(cfg$accdb_path)) {
  stop("No ACCDB file could be resolved. Set SOMEC_ACCDB_PATH or SOMEC_ACCDB_SUFFIX.", call. = FALSE)
}
if (!file.exists(cfg$accdb_path)) {
  stop("ACCDB file not found: ", cfg$accdb_path, call. = FALSE)
}

dir.create(cfg$cache_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# MODULES
# ============================================================

source(file.path(.repo_root, "qc_helpers.R"))
source(file.path(.repo_root, "qc_cross_validation.R"))

# ============================================================
# VERSION + FIX SCRIPT PATH
# ============================================================

.accdb_version <- cfg$accdb_version
.fixes_script  <- file.path(.repo_root, paste0("apply_qc_fixes_", .accdb_version, ".R"))

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
# ISSUE EXPLANATION
# ============================================================

explain_issue <- function(issue_row, mission_id) {

  tbl    <- issue_row$table
  col    <- issue_row$column
  df_mis <- get(tbl) |> filter(.data[["mission"]] == mission_id)

  cat("\n--------------------------------------------\n")
  cat(msg("mission_label"), mission_id, "\n")
  cat(msg("issue_label"),   issue_row$issue_type, "\n")
  cat(msg("variable_label"), paste0(tbl, "$", col), "\n")
  cat("--------------------------------------------\n\n")

  # CATEGORICAL
  if (issue_row$issue_type == "UNKNOWN_CATALOG") {
    cat(msg("miss_dist"), "\n")
    print_dist(df_mis[[col]])
    n_na <- sum(is.na(df_mis[[col]]))
    cat(msg("missing_na"), n_na, "\n\n")
    if (n_na > 0) {
      df_na <- show_rows_with_context(df_mis, col, which(is.na(df_mis[[col]])))
      view_in_viewer(df_na, title = paste0(col, " — ", msg("rows_missing")))
    }
    allowed    <- catalog_map[[paste0(tolower(tbl), "$", tolower(col))]]
    unknowns   <- strsplit(issue_row$details, ",\\s*")[[1]]
    recode_map <- character()
    cat("\n", msg("catalog_check"), "\n")
    for (u in unknowns) {
      cat(" \u274c", u, "\n")
      best <- allowed[which.min(stringdist(u, allowed, method = "lv"))]
      cat("    \U0001F4A1", msg("did_you_mean"), best, "\n")
      recode_map[u] <- best
    }
    snippets <- list()
    if (length(recode_map)) {
      recode_snippet <- generate_recode_snippet(tbl, col, recode_map)
      cat("\n", msg("suggested_fix"), "\n\n")
      cat(recode_snippet, "\n")
      snippets$recode <- recode_snippet
    }
    if (n_na > 0) {
      cat("\n", msg("na_options"), "\n")
      cat(msg("na_opt1"), "\n")
      cat(msg("na_opt4"), "\n")
      cat(msg("na_skip"), "\n\n")
      na_choice <- prompt(msg("na_prompt"), c("1", "4", ""))
      na_snippet <- if (na_choice == "1") {
        paste0("# NA decision [", col, "]: Leave as-is (NA kept)\n# No change applied.\n")
      } else if (na_choice == "4") {
        custom <- trimws(readline(paste0(msg("na_custom_prompt"), " ", col, ": ")))
        paste0("# NA decision [", col, "]: Custom value = ", custom, "\n",
               tbl, " <- ", tbl, " |>\n  mutate(\n    ", col,
               " = if_else(mission == \"", mission_id, "\" & is.na(", col, "),\n      \"",
               custom, "\", ", col, ")\n  )")
      } else NULL
      if (!is.null(na_snippet)) {
        cat("\n", msg("generated_fix"), "\n\n")
        cat(na_snippet, "\n")
        snippets$na <- na_snippet
      }
    }
    if (length(snippets) == 0) return(invisible(NULL))
    if (length(snippets) == 1) return(invisible(snippets[[1]]))
    return(invisible(snippets))
  }

  # NUMERIC
  if (issue_row$issue_type == "NUMERIC_OUTLIER") {
    x      <- suppressWarnings(as.numeric(df_mis[[col]]))
    n_na   <- sum(is.na(x))
    df_na  <- NULL
    df_out <- NULL

    cat(msg("miss_num_dist"), "\n")
    cat(msg("n_label"),       sum(!is.na(x)), "\n")
    cat(msg("missing_label"), n_na, "\n")
    cat(msg("min_label"),     min(x, na.rm = TRUE), "\n")
    cat(msg("median_label"),  median(x, na.rm = TRUE), "\n")
    cat(msg("max_label"),     max(x, na.rm = TRUE), "\n\n")

    if (n_na > 0)
      df_na <- show_rows_with_context(df_mis, col, which(is.na(x)))

    gb <- get_global_numeric_baseline(global_ctx, tbl, col)

    if (nrow(gb) > 0) {
      outliers <- x < gb$p05 | x > gb$p95
      cat(msg("global_ctx"), "\n")
      cat("  p05 :", gb$p05, "\n")
      cat("  p50 :", gb$p50, "\n")
      cat("  p95 :", gb$p95, "\n\n")
      cat(msg("outliers_label"), "\n")
      cat(msg("n_outliers"), sum(outliers, na.rm = TRUE), "\n")
      if (any(outliers, na.rm = TRUE)) {
        cat(msg("min_outlier"), min(x[outliers], na.rm = TRUE), "\n")
        cat(msg("max_outlier"), max(x[outliers], na.rm = TRUE), "\n\n")
        df_out <- show_rows_with_context(df_mis, col, which(!is.na(outliers) & outliers))
      }
      if (n_na == 0 && sum(outliers, na.rm = TRUE) == 0)
        cat("\n\u26a0\ufe0f ", if (.lang == "f")
          paste0("Aucun problème détecté. Détails : ", issue_row$details)
        else
          paste0("No issues found. Report details: ", issue_row$details), "\n\n")

      line_labels <- if (.lang == "f")
        c("p05 / p95 (global)", "p50 médiane (global)")
      else
        c("p05 / p95 (global)", "p50 median (global)")

      plot_df <- tibble(
        value    = ifelse(is.na(x), gb$p50, x),
        category = case_when(
          is.na(x)                  ~ if (.lang == "f") "Manquant (NA)" else "Missing (NA)",
          coalesce(outliers, FALSE) ~ if (.lang == "f") "Aberrant"      else "Outlier",
          TRUE                      ~ "Normal"
        )
      )
      p <- ggplot(plot_df, aes(x = 0, y = value)) +
        geom_jitter(aes(color = category, shape = category), width = 0.15, size = 2, alpha = 0.7) +
        geom_hline(aes(yintercept = gb$p05, linetype = line_labels[1]), color = "steelblue", linewidth = 0.8) +
        geom_hline(aes(yintercept = gb$p95, linetype = line_labels[1]), color = "steelblue", linewidth = 0.8) +
        geom_hline(aes(yintercept = gb$p50, linetype = line_labels[2]), color = "grey40",    linewidth = 0.6) +
        scale_color_manual(values = c(
          "Normal" = "grey60", "Outlier" = "firebrick", "Aberrant" = "firebrick",
          "Missing (NA)" = "orange", "Manquant (NA)" = "orange")) +
        scale_shape_manual(values = c(
          "Normal" = 16, "Outlier" = 16, "Aberrant" = 16,
          "Missing (NA)" = 4, "Manquant (NA)" = 4)) +
        scale_linetype_manual(values = c("dashed", "dotted") |> setNames(line_labels)) +
        labs(title    = paste0(if (.lang == "f") "QC Numérique — " else "Numeric QC — ", tbl, "$", col),
             subtitle = paste0(if (.lang == "f") "Mission : " else "Mission: ", mission_id,
                               "  |  p05=", gb$p05, "  p50=", gb$p50, "  p95=", gb$p95,
                               "  |  NA=", n_na),
             y = col, x = NULL, color = NULL, shape = NULL, linetype = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

      .plot_file <- file.path(tempdir(), "qc_numeric_plot.png")
      ggsave(.plot_file, plot = p, width = 4, height = 6, dpi = 120)
    } else {
      cat("\u26a0\ufe0f ", msg("no_baseline"), "\n")
      outliers   <- rep(FALSE, length(x))
      .plot_file <- NULL
    }

    df_viewer <- if (!is.null(df_na) && !is.null(df_out)) bind_rows(df_na, df_out)
                 else if (!is.null(df_na))  df_na
                 else if (!is.null(df_out)) df_out
                 else                       NULL

    view_in_viewer(df = df_viewer, title = paste0(tbl, "$", col),
                   plot_file = if (exists(".plot_file")) .plot_file else NULL)

    cat("\n", msg("qa_actions"), "\n")
    cat(msg("qa_units"), if (nrow(gb) > 0) gb$p50 else "N/A", ")\n")
    cat(msg("qa_source"), "\n")
    cat(msg("qa_cap"), "\n")

    return(invisible(list(
      type = "NUMERIC_OUTLIER", x = x, n_na = n_na, outliers = outliers,
      tbl = tbl, col = col, mission_id = mission_id, df_mis = df_mis,
      p50 = if (nrow(gb) > 0) gb$p50 else NA
    )))
  }

  # DATETIME
  if (issue_row$issue_type == "OUTSIDE_MISSION_DATES") {
    raw_bounds <- get_mission_bounds(missions, mission_id)
    mis_start  <- as.POSIXct(paste(as.Date(raw_bounds$start), "00:00:00"), tz = "UTC")
    mis_end    <- as.POSIXct(paste(as.Date(raw_bounds$end),   "23:59:59"), tz = "UTC")
    outside    <- df_mis |> filter(!is.na(.data[[col]]) &
                    (.data[[col]] < mis_start | .data[[col]] > mis_end))
    n_na <- sum(is.na(df_mis[[col]]))

    cat(msg("bounds_label"), "\n")
    cat(msg("start_label"), format(mis_start, "%Y-%m-%d %H:%M:%S"), "\n")
    cat(msg("end_label"),   format(mis_end,   "%Y-%m-%d %H:%M:%S"), "\n")
    cat(msg("missing_label"), n_na, "\n\n")

    if (n_na > 0)
      view_in_viewer(show_rows_with_context(df_mis, col, which(is.na(df_mis[[col]]))),
                     title = paste0(col, " — ", msg("rows_missing")))
    if (nrow(outside) > 0)
      view_in_viewer(show_rows_with_context(df_mis, col,
                       which(!is.na(df_mis[[col]]) &
                         (df_mis[[col]] < mis_start | df_mis[[col]] > mis_end))),
                     title = paste0(col, " — ", msg("offending_label")))

    dt_vals <- as.POSIXct(outside[[col]], origin = "1970-01-01", tz = "UTC")
    cat(msg("offending_label"), "\n")
    cat(msg("count_label"),   length(dt_vals), "\n")
    if (length(dt_vals) > 0) {
      cat(msg("earliest_label"), format(min(dt_vals), "%Y-%m-%d %H:%M:%S"), "\n")
      cat(msg("latest_label"),   format(max(dt_vals), "%Y-%m-%d %H:%M:%S"), "\n")
    }

    snippet <- paste0(
      tbl, " <- ", tbl, " |>\n  mutate(\n    ", col, " = if_else(\n",
      "      mission == \"", mission_id, "\" &\n",
      "      (", col, " < as.POSIXct(\"", format(mis_start, "%Y-%m-%d %H:%M:%S"), "\") |\n",
      "       ", col, " > as.POSIXct(\"", format(mis_end,   "%Y-%m-%d %H:%M:%S"), "\")),\n",
      "      as.POSIXct(paste(as.Date(date), format(", col, ", \"%H:%M:%S\")), tz=\"UTC\"),\n",
      "      ", col, "\n    )\n  )"
    )
    cat("\n\u2705", msg("fix_common"), "\n\n")
    cat(snippet, "\n")
    return(invisible(snippet))
  }
}

# ============================================================
# NUMERIC FIX MENU
# ============================================================

build_numeric_snippet <- function(ctx) {
  x <- ctx$x; n_na <- ctx$n_na; tbl <- ctx$tbl
  col <- ctx$col; mission_id <- ctx$mission_id; outliers <- ctx$outliers
  na_snippet <- NULL; outlier_snippet <- NULL

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
    .dl <- if (.lang == "f")
      c("1" = "Laisser tel quel (NA conservé)",
        "2" = paste0("Remplacer par la moyenne (", mis_mean, ")"),
        "3" = paste0("Remplacer par la médiane (", mis_median, ")",),
        "4" = "Valeur personnalisée")
    else
      c("1" = "Leave as-is (NA kept)",
        "2" = paste0("Replace with mean (", mis_mean, ")"),
        "3" = paste0("Replace with median (", mis_median, ")"),
        "4" = "Custom value")
    na_snippet <- if (na_choice == "1") {
      paste0("# NA decision [", col, "]: ", .dl["1"], "\n# No change applied.\n")
    } else if (na_choice == "2") {
      paste0("# NA decision [", col, "]: ", .dl["2"], "\n",
             tbl, " <- ", tbl, " |>\n  mutate(\n    ", col,
             " = if_else(mission == \"", mission_id, "\" & is.na(", col, "),\n      ",
             mis_mean, ", ", col, ")\n  )")
    } else if (na_choice == "3") {
      paste0("# NA decision [", col, "]: ", .dl["3"], "\n",
             tbl, " <- ", tbl, " |>\n  mutate(\n    ", col,
             " = if_else(mission == \"", mission_id, "\" & is.na(", col, "),\n      ",
             mis_median, ", ", col, ")\n  )")
    } else if (na_choice == "4") {
      custom <- trimws(readline(paste0(msg("na_custom_prompt"), " ", col, ": ")))
      paste0("# NA decision [", col, "]: ", .dl["4"], " = ", custom, "\n",
             tbl, " <- ", tbl, " |>\n  mutate(\n    ", col,
             " = if_else(mission == \"", mission_id, "\" & is.na(", col, "),\n      ",
             custom, ", ", col, ")\n  )")
    } else NULL
    if (!is.null(na_snippet)) { cat("\n", msg("generated_fix"), "\n\n"); cat(na_snippet, "\n") }
  }

  if (any(outliers, na.rm = TRUE)) {
    outlier_snippet <- paste0(tbl, " <- ", tbl, " |>\n  mutate(\n    ", col,
                               " = ", col, "  ", msg("code_reviewer"), "\n  )")
    cat(msg("template_label"), "\n\n"); cat(outlier_snippet, "\n")
  }

  snippets <- list()
  if (!is.null(na_snippet))      snippets$na      <- na_snippet
  if (!is.null(outlier_snippet)) snippets$outlier <- outlier_snippet
  if (length(snippets) == 0) return(NULL)
  snippets
}

# ============================================================
# MISSION SELECTION
# ============================================================

.select_missions <- function(missions_list) {
  cat("\n")
  cat(if (.lang == "f") "SELECTION DES MISSIONS :\n" else "MISSION SELECTION:\n")
  cat(if (.lang == "f")
    "  [1] Commencer depuis le debut\n  [2] Commencer a une mission specifique\n  [3] Traiter une seule mission\n"
  else
    "  [1] Start from the beginning\n  [2] Start at a specific mission\n  [3] Process one mission only\n")
  cat("\n")
  sel <- prompt(if (.lang == "f") "Choix [1/2/3] : " else "Choice [1/2/3]: ", c("1", "2", "3"))
  if (sel == "1") return(missions_list)
  cat("\n")
  for (i in seq_along(missions_list)) cat(sprintf("  [%3d] %s\n", i, missions_list[i]))
  idx <- suppressWarnings(as.integer(trimws(readline(
    if (sel == "2")
      if (.lang == "f") "Numero de la mission de depart : " else "Starting mission number: "
    else
      if (.lang == "f") "Numero de la mission : " else "Mission number: "
  ))))
  if (is.na(idx) || idx < 1 || idx > length(missions_list)) {
    cat(if (.lang == "f") "Choix invalide - depuis le debut.\n" else "Invalid - starting from beginning.\n")
    return(missions_list)
  }
  if (sel == "2") return(missions_list[idx:length(missions_list)])
  return(missions_list[idx])
}

# ============================================================
# INTERACTIVE LOOP
# ============================================================

# ============================================================
# MISSION SELECTION
# ============================================================

.select_missions <- function(missions_list) {
  cat("\n")
  cat(if (.lang == "f") "SELECTION DES MISSIONS :\n" else "MISSION SELECTION:\n")
  cat(if (.lang == "f")
    "  [1] Commencer depuis le debut\n  [2] Commencer a une mission specifique\n  [3] Traiter une seule mission\n"
  else
    "  [1] Start from the beginning\n  [2] Start at a specific mission\n  [3] Process one mission only\n")
  cat("\n")
  sel <- prompt(if (.lang == "f") "Choix [1/2/3] : " else "Choice [1/2/3]: ", c("1", "2", "3"))
  if (sel == "1") return(missions_list)
  cat("\n")
  for (i in seq_along(missions_list)) cat(sprintf("  [%3d] %s\n", i, missions_list[i]))
  idx <- suppressWarnings(as.integer(trimws(readline(
    if (sel == "2")
      if (.lang == "f") "Numero de la mission de depart : " else "Starting mission number: "
    else
      if (.lang == "f") "Numero de la mission : " else "Mission number: "
  ))))
  if (is.na(idx) || idx < 1 || idx > length(missions_list)) {
    cat(if (.lang == "f") "Choix invalide - depuis le debut.\n" else "Invalid - starting from beginning.\n")
    return(missions_list)
  }
  if (sel == "2") return(missions_list[idx:length(missions_list)])
  return(missions_list[idx])
}

# ============================================================
# INTERACTIVE LOOP
# ============================================================

run_interactive_qc <- function() {

  all_missions  <- unique(mission_issues$mission)
  missions_list <- .select_missions(all_missions)

  for (m_idx in seq_along(missions_list)) {

    mis    <- missions_list[m_idx]
    issues <- mission_issues |> filter(.data[["mission"]] == mis)

    cat("\n================================================\n")
    cat(msg("mission_label"), mis, "\n")
    cat("================================================\n")

    for (j in seq_len(nrow(issues))) {

      issue  <- issues[j, ]
      df_mis <- get(issue$table) |> filter(.data[["mission"]] == mis)
      n_na   <- sum(is.na(df_mis[[issue$column]]))
      na_tag <- if (n_na > 0) paste0("  [", n_na, " NA]") else ""

      cat(sprintf("\n[%d/%d] %s — %s$%s%s\n",
          j, nrow(issues), issue$issue_type, issue$table, issue$column, na_tag))

      choice <- prompt(msg("opt_view"), c("v", "s", "n", "q"))

      if (choice == "v") {
        .result  <- explain_issue(issue, mis)
        # Numeric: ctx list with type="NUMERIC_OUTLIER"
        # Categorical with both recode+NA: named list (snippets)
        # Other: plain string snippet
        ctx      <- if (!is.null(.result) && is.list(.result) &&
                        identical(.result$type, "NUMERIC_OUTLIER")) .result else NULL
        snippets <- if (!is.null(.result) && is.list(.result) && is.null(ctx)) .result else NULL
        snippet  <- if (!is.null(.result) && is.character(.result)) .result else NULL
        cat("\n")
        df_mis <- get(issue$table) |> filter(.data[["mission"]] == mis)

        repeat {
          is_numeric   <- !is.null(ctx)
          has_snippet  <- !is.null(snippet)
          has_snippets <- !is.null(snippets) && length(snippets) > 0

          menu_msg <- if (is_numeric && is.null(snippets)) {
            if (.lang == "f") "Options : [d]ate inspecter / [f]ixer valeurs / [Entrée] continuer > "
            else              "Options: [d]ay inspect / [f]ix missing values / [Enter] continue > "
          } else if (has_snippet || has_snippets) {
            if (.lang == "f") "Options : [a]jouter correction / [Entrée] continuer > "
            else              "Options: [a]dd fix to script / [Enter] continue > "
          } else {
            if (.lang == "f") "Options : [d]ate inspecter / [Entrée] continuer > "
            else              "Options: [d]ay inspect / [Enter] continue > "
          }

          valid_opt <- if (is_numeric && is.null(snippets)) c("d", "f", "")
                       else if (has_snippet || has_snippets) c("a", "")
                       else c("d", "")

          post <- prompt(menu_msg, valid_opt)

          if (post == "d") {
            show_rows_for_day(df_mis, issue$column)
          } else if (post == "f" && is_numeric) {
            snippets <- build_numeric_snippet(ctx)
            if (is.null(snippets))
              cat(if (.lang == "f") "  Aucune correction à apporter.\n" else "  Nothing to fix.\n")
          } else if (post == "a") {
            if (has_snippet) {
              append_fix(snippet, mis, issue$issue_type, issue$table, issue$column)
              snippet <- NULL
            } else if (has_snippets) {
              for (sname in names(snippets)) {
                label <- if (sname == "na")
                  if (.lang == "f") "correction NA" else "NA fix"
                else
                  if (.lang == "f") "correction aberrants" else "outlier fix"
                add <- prompt(
                  if (.lang == "f")
                    paste0("  Ajouter ", label, " ? [a]jouter / [Entrée] passer > ")
                  else
                    paste0("  Add ", label, "? [a]dd / [Enter] skip > "),
                  c("a", ""))
                if (add == "a")
                  append_fix(snippets[[sname]], mis,
                             paste0(issue$issue_type, "_", toupper(sname)),
                             issue$table, issue$column)
              }
              snippets <- NULL
            }
            break
          } else {
            break
          }
        }
      }

      if (choice == "n") break
      if (choice == "q") return(invisible(NULL))
    }

    cat(if (.lang == "f")
      paste0("\n--- Validation croisée : ", mis, " ---\n")
    else
      paste0("\n--- Cross-validation: ", mis, " ---\n"))

    run_cv_for_mission(mis)

    if (m_idx < length(missions_list)) {
      next_mis <- missions_list[m_idx + 1]
      go_next  <- prompt(
        if (.lang == "f")
          paste0("\n\u2705  Mission ", mis, " terminée. Passer à ", next_mis, " ? [o]ui / [q]uitter > ")
        else
          paste0("\n\u2705  Mission ", mis, " done. Go to ", next_mis, "? [y]es / [q]uit > "),
        if (.lang == "f") c("o", "q") else c("y", "q"))
      if (go_next == "q") return(invisible(NULL))
    }
  }

  cat("\n\u2705 ", msg("session_complete"), "\n")
  if (file.exists(.fixes_script))
    cat("\U0001F4C4 ", msg("fix_script_label"), .fixes_script, "\n")
}

# ============================================================
# RUN
# ============================================================

run_interactive_qc()
