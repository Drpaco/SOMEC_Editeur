# ============================================================
# FILE: interactive_mission_qc_optionA.R
# PURPOSE:
#   Interactive console-based QA reviewer
#   - Categorical, datetime, and numeric QA
#   - Read-only: explains issues and PRINTS fix suggestions
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(RODBC)
  library(stringdist)
})

# ============================================================
# CONFIG
# ============================================================

# Always resolve paths relative to this script's location (repo root),
# regardless of the working directory on any platform.
.repo_root <- tryCatch(
  # Works when script is sourced via source()
  frames <- sys.frames()
  ofiles <- Filter(Negate(is.null), lapply(frames, function(f) f$ofile))
  if (length(ofiles) > 0) {
    dirname(normalizePath(ofiles[[1]], mustWork = TRUE))
  } else {
    # Fallback for RStudio/Positron interactive use
    dirname(normalizePath(
      rstudioapi::getSourceEditorContext()$path, mustWork = TRUE
    ))
  }
}, error = function(e) getwd())

# Folder structure (same on Windows and Mac):
#   SOMEC/
#   ├── BaseDeDonnees/           <- .accdb lives here
#   └── GestionDeDonnees/
#       ├── GlobalContext/       <- context files
#       ├── MissionReports/
#       └── SOMEC_Editeur/       <- THIS repo (.repo_root)
#           └── _cache/          <- .rds cache (committed to git)

.gestion_root       <- dirname(.repo_root)                          # GestionDeDonnees/
.somec_root         <- dirname(.gestion_root)                       # SOMEC/
.cache_dir          <- file.path(.repo_root, "_cache")

cfg <- list(
  accdb_path          = file.path(.somec_root, "BaseDeDonnees", "SOMEC_20251106.accdb"),
  context_dir         = file.path(.gestion_root, "GlobalContext"),
  mission_reports_dir = file.path(.gestion_root, "MissionReports"),
  cache_dir           = .cache_dir
)

dir.create(cfg$cache_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# HELPERS
# ============================================================

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

prompt <- function(msg, valid = NULL) {
  repeat {
    ans <- readline(msg)
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

# ============================================================
# LOAD DATA
# ============================================================

missions     <- load_access_table_cached(cfg$accdb_path, "missions", cfg$cache_dir) |> clean_names()
transects    <- load_access_table_cached(cfg$accdb_path, "transects", cfg$cache_dir) |> clean_names()
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
  
  tbl <- issue_row$table
  col <- issue_row$column
  df_mis <- get(tbl) |> filter(mission == mission_id)
  
  cat("\n--------------------------------------------\n")
  cat("Mission :", mission_id, "\n")
  cat("Issue   :", issue_row$issue_type, "\n")
  cat("Variable:", paste0(tbl, "$", col), "\n")
  cat("--------------------------------------------\n\n")
  
  # ============================================================
  # CATEGORICAL — UNKNOWN CATALOG
  # ============================================================
  
  if (issue_row$issue_type == "UNKNOWN_CATALOG") {
    
    cat("MISSION DISTRIBUTION:\n")
    print_dist(df_mis[[col]])
    
    allowed <- catalog_map[[paste0(tolower(tbl), "$", tolower(col))]]
    unknowns <- strsplit(issue_row$details, ",\\s*")[[1]]
    
    recode_map <- character()
    
    cat("\nCATALOG CHECK:\n")
    for (u in unknowns) {
      cat(" ❌", u, "not in catalog\n")
      d <- stringdist(u, allowed, method = "lv")
      best <- allowed[which.min(d)]
      cat("    💡 Did you mean:", best, "\n")
      recode_map[u] <- best
    }
    
    if (length(recode_map)) {
      cat("\nSUGGESTED FIX (R code):\n\n")
      cat(generate_recode_snippet(tbl, col, recode_map), "\n")
    }
    return(invisible(NULL))
  }
  
  # ============================================================
  # NUMERIC — OUTLIERS USING GLOBAL CONTEXT
  # ============================================================
  
  if (issue_row$issue_type == "NUMERIC_OUTLIER") {
    
    x <- suppressWarnings(as.numeric(df_mis[[col]]))
    
    cat("MISSION NUMERIC DISTRIBUTION:\n")
    cat("  n      :", sum(!is.na(x)), "\n")
    cat("  min    :", min(x, na.rm = TRUE), "\n")
    cat("  median :", median(x, na.rm = TRUE), "\n")
    cat("  max    :", max(x, na.rm = TRUE), "\n\n")
    
    gb <- get_global_numeric_baseline(global_ctx, tbl, col)
    
    if (nrow(gb) == 0) {
      cat("⚠️ No global numeric baseline available.\n")
      return(invisible(NULL))
    }
    
    cat("GLOBAL CONTEXT (numeric baselines):\n")
    cat("  p05 :", gb$p05, "\n")
    cat("  p50 :", gb$p50, "\n")
    cat("  p95 :", gb$p95, "\n\n")
    
    outliers <- x < gb$p05 | x > gb$p95
    
    cat("OUTLIERS IN THIS MISSION:\n")
    cat("  n outliers:", sum(outliers, na.rm = TRUE), "\n")
    
    if (any(outliers, na.rm = TRUE)) {
      cat("  min outlier:", min(x[outliers], na.rm = TRUE), "\n")
      cat("  max outlier:", max(x[outliers], na.rm = TRUE), "\n\n")
      
      cat("OUTLIER ROWS:\n")
      outlier_rows <- df_mis[!is.na(outliers) & outliers, ]
      print(outlier_rows, n = Inf)
      
      plot_df <- tibble(value = x, is_outlier = outliers)
      p <- ggplot(plot_df, aes(x = 0, y = value)) +
        geom_jitter(aes(color = is_outlier), width = 0.15, size = 2, alpha = 0.7) +
        geom_hline(yintercept = gb$p05, linetype = "dashed", color = "steelblue",
                   linewidth = 0.8) +
        geom_hline(yintercept = gb$p95, linetype = "dashed", color = "steelblue",
                   linewidth = 0.8) +
        geom_hline(yintercept = gb$p50, linetype = "dotted", color = "grey40",
                   linewidth = 0.6) +
        scale_color_manual(
          values = c("FALSE" = "grey60", "TRUE" = "firebrick"),
          labels = c("FALSE" = "Normal", "TRUE" = "Outlier")
        ) +
        labs(
          title = paste0("Numeric QC — ", tbl, "$", col),
          subtitle = paste0("Mission: ", mission_id,
                            "  |  Global p05=", gb$p05,
                            "  p50=", gb$p50,
                            "  p95=", gb$p95),
          y = col, x = NULL, color = NULL
        ) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      print(p)
    }
    
    cat("\nSUGGESTED QA ACTIONS:\n")
    cat("  • Verify units (global median:", gb$p50, ")\n")
    cat("  • Inspect source records\n")
    cat("  • Decide if capping or leaving as-is is appropriate\n\n")
    
    cat("OPTIONAL TEMPLATE (NOT APPLIED):\n\n")
    cat(
      tbl, " <- ", tbl, " |>\n",
      "  mutate(\n",
      "    ", col, " = ", col, "  # reviewer decision\n",
      "  )\n",
      sep = ""
    )
    return(invisible(NULL))
  }
  
  # ============================================================
  # DATETIME — OUTSIDE MISSION DATES
  # ============================================================
  
  if (issue_row$issue_type == "OUTSIDE_MISSION_DATES") {
    
    raw_bounds <- get_mission_bounds(missions, mission_id)
    
    # Date-normalized mission bounds
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
    
    cat("MISSION BOUNDS (date-normalized):\n")
    cat("  start:", format(mis_start, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("  end  :", format(mis_end,   "%Y-%m-%d %H:%M:%S"), "\n\n")
    
    dt_vals <- as.POSIXct(outside[[col]], origin = "1970-01-01", tz = "UTC")
    
    cat("OFFENDING RECORDS:\n")
    cat("  count   :", length(dt_vals), "\n")
    if (length(dt_vals) > 0) {
      cat("  earliest:", format(min(dt_vals), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("  latest  :", format(max(dt_vals), "%Y-%m-%d %H:%M:%S"), "\n")
    }
    
    cat("\n✅ MOST COMMON FIX (row-level date + time):\n\n")
    
    cat(
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
      "  )\n\n",
      sep = ""
    )
    
    cat("ALTERNATIVE (hour-only semantics):\n\n")
    cat(
      tbl, " <- ", tbl, " |>\n",
      "  mutate(\n",
      "    ", col, " = as.POSIXct(paste(\"1970-01-01\", format(", col, ", \"%H:%M:%S\")), tz=\"UTC\")\n",
      "  )\n",
      sep = ""
    )
  }
}

# ============================================================
# INTERACTIVE LOOP
# ============================================================

run_interactive_qc <- function() {
  
  for (mis in unique(mission_issues$mission)) {
    
    cat("\n================================================\n")
    cat("Mission:", mis, "\n")
    cat("================================================\n")
    
    issues <- mission_issues |> filter(mission == mis)
    
    for (j in seq_len(nrow(issues))) {
      
      issue <- issues[j, ]
      
      cat(sprintf(
        "\n[%d] %s — %s$%s\n",
        j, issue$issue_type, issue$table, issue$column
      ))
      
      choice <- prompt(
        "Options: [v]iew details / [s]kip / [n]ext mission / [q]uit > ",
        c("v", "s", "n", "q")
      )
      
      if (choice == "v") {
        explain_issue(issue, mis)
        prompt("\nPress [Enter] to return...")
      }
      
      if (choice == "n") break
      if (choice == "q") return(invisible(NULL))
    }
  }
}


# ============================================================
# RUN
# ============================================================

run_interactive_qc()
