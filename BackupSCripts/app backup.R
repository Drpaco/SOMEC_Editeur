# ============================================================
# app.R — SOMEC Interactive QA & Editor (FULL FILE)
# - Left: QC report browser + buttons to run loader/profiler
# - Right: Console log + live editable views of missions/transects/observations
# - Save edits to a COPY of the Access DB (.accdb)
# - Loader/Profiler run in external R sessions (callr) with ODBC pre-flight
# ============================================================

library(shiny)
library(DT)
library(openxlsx)
library(RODBC)
library(dplyr)
library(callr)

# ----------------------- DEFAULT PATHS (EDIT THESE) -----------------------
default_loader   <- "C:/Users/BolducF/Documents/ShinyApps/SOMEC_Editeur/catalog_loader.R"
default_profiler <- "C:/Users/BolducF/Documents/ShinyApps/SOMEC_Editeur/mission_profiler_somerc_v6_3_4.R"
default_accdb    <- "C:/Users/BolducF/Documents/ShinyApps/SOMEC/BaseDeDonnees/SOMEC_20251106.accdb"
default_reports  <- 'C:/Users/BolducF/Documents/ShinyApps/SOMEC/GestionDeDonnees/'
# -------------------------------------------------------------------------

# ---------- helpers (global) ----------
is_dt <- function(x) inherits(x, "POSIXt") || inherits(x, "Date")

# Quote for SQL (Access)
sql_literal <- function(x) {
  if (is.na(x)) return("NULL")
  if (inherits(x, "Date")) return(paste0("#", format(x, "%Y-%m-%d"), "#"))
  if (is.numeric(x)) return(as.character(x))
  paste0("'", gsub("'", "''", as.character(x)), "'")
}

# Build UPDATE for Access
build_update_sql <- function(tbl, key_vals, set_vals) {
  set_clauses   <- paste0("[", names(set_vals), "] = ", vapply(set_vals, sql_literal, character(1)))
  where_clauses <- paste0("[", names(key_vals), "] = ", vapply(key_vals, sql_literal, character(1)))
  paste0("UPDATE [", tbl, "] SET ", paste(set_clauses, collapse = ", "),
         " WHERE ", paste(where_clauses, collapse = " AND "), ";")
}

# ----------------------- UI -----------------------
ui <- fluidPage(
  # ── EXACT POSITION: first child of fluidPage ──────────────────────────────
  tags$head(
    # CSS to keep the page stable and the console usable
    tags$style(HTML("
      .tab-content { min-height: 360px; }
      pre#console_log { max-height: 45vh; overflow: auto; }
    ")),
    # JS: when a tab becomes visible, adjust DataTables columns to avoid flicker
    tags$script(HTML("
      $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function (e) {
        $('.dataTable').each(function(){
          try { $(this).DataTable().columns.adjust(); } catch(e) {}
        });
      });
    "))
  ),
  # ──────────────────────────────────────────────────────────────────────────
  
  titlePanel("SOMEC — QA & Edit"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Run / Source"),
      textInput("loader_path", "Loader file (catalog_loader.R):", value = default_loader, width = "100%"),
      actionButton("run_loader", "Run Loader"),
      tags$hr(),
      textInput("profiler_path", "Profiler file (mission_profiler...R):", value = default_profiler, width = "100%"),
      selectizeInput("missions_to_run", "Missions to profile (empty = all)", choices = NULL, multiple = TRUE),
      textInput("reports_parent", "Reports parent folder:", value = default_reports, width = "100%"),
      actionButton("run_profiler_sel", "Run Profiler (selected)"),
      actionButton("run_profiler_all", "Run Profiler (ALL)"),
      tags$hr(),
      h4("Access DB"),
      textInput("accdb_path", "Access (.accdb):", value = default_accdb, width = "100%"),
      textInput("accdb_copy", "Save edits to COPY (.accdb):",
                value = file.path(dirname(default_accdb), "SOMEC_EDIT_COPY.accdb"), width = "100%"),
      actionButton("save_edits", "Save edits to COPY"),
      tags$hr(),
      h4("QC Reports"),
      uiOutput("reports_folder_ui"),
      uiOutput("report_file_ui")
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",  # ← EXACT ADDITION: give the tabset an id for the JS above
        tabPanel(
          "Console log",
          div(
            style = "padding:8px; white-space:pre-wrap;",
            verbatimTextOutput("console_log", placeholder = TRUE)
          )
        ),
        tabPanel(
          "QC Summary viewer",
          # --- Top row: report selector + refresh button ---
          fluidRow(
            column(
              width = 9,
              selectizeInput(
                inputId  = "report_picker",
                label    = "Select report (.xlsx):",
                choices  = NULL,
                multiple = FALSE,
                options  = list(placeholder = "Pick an .xlsx report"),
                width    = "100%"
              )
            ),
            column(
              width = 3,
              div(
                style = "margin-top:26px;",
                actionButton("refresh_reports", "Refresh list")
              )
            )
          ),
          div(
            id    = "qc_status_bar",
            textOutput("qc_status"),
            style = "margin-top:4px; color:#555; font-size:12px;"
          ),
          
          tags$hr(),
          # --- The viewer itself ---
          fluidRow(
            column(12, DTOutput("qc_summary_dt"), style = "margin-top:10px;")
          )
        ),
        tabPanel(
          "Data editor",
          fluidRow(column(4, selectInput("mission_filter", "Mission to edit:", choices = NULL))),
          tabsetPanel(
            tabPanel("Missions",     DTOutput("missions_dt")),
            tabPanel("Transects",    DTOutput("transects_dt")),
            tabPanel("Observations", DTOutput("observations_dt"))
          )
        )
      )
    )
  )
)



# ----------------------- SERVER -----------------------
server <- function(input, output, session) {
  
  # Reactive containers
  rv <- reactiveValues(
    missions = NULL, transects = NULL, observations = NULL,
    missions_edit = NULL, transects_edit = NULL, observations_edit = NULL,
    changes = list(),
    cats_loaded = FALSE
  )
  
  # ---- Console log (after rv <- reactiveValues(...)) ----
  rv$log <- reactiveVal("")
  # Single source of truth for the currently selected report file
  rv$selected_report <- reactiveVal(NULL)
  
  append_log <- function(...) {
    txt <- paste(format(Sys.time(), "%H:%M:%S"), paste(..., collapse = " "), sep = "  ")
    current <- rv$log()
    keep <- if (nzchar(current)) substr(current, max(1, nchar(current) - 10000), nchar(current)) else ""
    rv$log(paste0(keep, if (nzchar(keep)) "\n" else "", txt))
  }
  output$console_log <- renderText(rv$log())
  
  # ---- Log cursor: read ONLY new lines from a log file ----
  `%||%` <- function(x, y) if (is.null(x)) y else x
  rv$log_index <- list()  # path -> last line shown
  read_new_log_lines <- function(logf) {
    if (!file.exists(logf)) return(character(0))
    all <- readLines(logf, warn = FALSE)
    prev <- rv$log_index[[logf]] %||% 0L
    if (length(all) <= prev) return(character(0))
    new <- all[(prev + 1L):length(all)]
    rv$log_index[[logf]] <<- length(all)
    new
  }
  
  # ---- One‑shot external runner (child R via callr, logs to file; tails it; destroys itself) ----
  run_external_r <- function(tag, lines, on_done = NULL) {
    wrapper <- tempfile(fileext = ".R")
    writeLines(lines, wrapper)
    
    logf <- tempfile(fileext = ".log")
    rv$log_index[[logf]] <- 0L
    append_log(sprintf("[%s] wrapper: %s | log: %s", tag, wrapper, logf))
    
    # Child R: stdout/stderr redirected by callr (no sink/unlink needed)
    p <- callr::r_bg(
      function(wrap) { source(wrap, local = FALSE) },
      args   = list(wrap = wrapper),
      stdout = logf,
      stderr = logf
    )
    
    obs <- NULL
    obs <- observe({
      invalidateLater(1000, session)
      
      out <- read_new_log_lines(logf)
      if (length(out)) append_log(paste(out, collapse = "\n"))
      
      if (!p$is_alive()) {
        out <- read_new_log_lines(logf)
        if (length(out)) append_log(paste(out, collapse = "\n"))
        
        append_log(sprintf("[%s] finished. Exit status: %s", tag, p$get_exit_status()))
        nice_tag <- paste0(toupper(substr(tag, 1, 1)), substr(tag, 2, nchar(tag)))
        showNotification(sprintf("%s finished.", nice_tag), type = "message")
        
        if (is.function(on_done)) try(on_done(), silent = TRUE)
        if (!is.null(obs)) obs$destroy()
      }
    })
  }
  
  # ---- Auto‑select the newest MissionReports_YYYYMMDD and a report file ----
  select_latest_report <- function(parent) {
    parent <- tryCatch(normalizePath(parent, winslash = "/", mustWork = FALSE), error = function(e) parent)
    if (!dir.exists(parent)) { append_log("[reports] Parent not found: ", parent); return(invisible(FALSE)) }
    
    subs <- list.dirs(parent, full.names = TRUE, recursive = FALSE)
    subs <- subs[grepl("MissionReports_[0-9]{8}$", basename(subs))]
    if (!length(subs)) { append_log("[reports] No MissionReports_YYYYMMDD under: ", parent); return(invisible(FALSE)) }
    
    dates <- suppressWarnings(as.integer(gsub("[^0-9]", "", basename(subs))))
    sel_dir <- subs[order(dates, decreasing = TRUE)][1]
    
    files <- list.files(sel_dir, pattern = "\\.xlsx$", full.names = TRUE)
    if (!length(files)) { append_log("[reports] No .xlsx in: ", sel_dir); return(invisible(FALSE)) }
    
    idx <- grep("SOMEC_Mission_QAQC_Index_", basename(files))
    sel_file <- if (length(idx)) files[idx[1]] else files[1]
    
    updateSelectInput(session, "reports_folder", selected = sel_dir)
    updateSelectInput(session, "report_file",   selected = sel_file)
    append_log("[reports] Selected folder: ", sel_dir)
    append_log("[reports] Selected file:   ", sel_file)
    invisible(TRUE)
  }
  
  build_report_choices <- function(parent) {
    parent <- tryCatch(normalizePath(parent, winslash = "/", mustWork = FALSE), error = function(e) parent)
    if (!dir.exists(parent)) return(character(0))
    
    # All MissionReports (newest first)
    subs <- list.dirs(parent, full.names = TRUE, recursive = FALSE)
    subs <- subs[grepl("MissionReports_[0-9]{8}$", basename(subs), ignore.case = TRUE)]
    if (!length(subs)) return(character(0))
    subs <- subs[order(basename(subs), decreasing = TRUE)]
    
    # Collect .xlsx under these folders (case-insensitive) and normalize to forward slashes
    files <- unlist(
      lapply(subs, function(d) list.files(d, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)),
      use.names = FALSE
    )
    if (!length(files)) return(character(0))
    files <- tryCatch(normalizePath(files, winslash = "/", mustWork = FALSE), error = function(e) files)
    
    # Label = <FolderName> • <FileName> ; value = full normalized path
    labs <- paste0(basename(dirname(files)), " • ", basename(files))
    stats::setNames(files, labs)
  }
  
  # ---- Access loader for the three base tables (missions/transects/observations) ----
  load_from_access <- function() {
    ap <- input$accdb_path
    if (!isTruthy(ap) || !file.exists(ap)) {
      append_log("[access] ACCDB not found:", ap); return(invisible(FALSE))
    }
    append_log("[access] Opening:", ap)
    con <- try(RODBC::odbcConnectAccess2007(ap, believeNRows = FALSE), silent = TRUE)
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
      append_log("[access] odbcConnectAccess2007 failed. Check driver/bitness/path.")
      return(invisible(FALSE))
    }
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    try({ rv$missions     <- if (exists("missions", .GlobalEnv)) get("missions", .GlobalEnv) else sqlFetch(con, "missions") })
    try({ rv$transects    <- if (exists("transects", .GlobalEnv)) get("transects", .GlobalEnv) else sqlFetch(con, "transects") })
    try({ rv$observations <- if (exists("observations", .GlobalEnv)) get("observations", .GlobalEnv) else sqlFetch(con, "observations") })
    append_log("[access] Data frames refreshed.")
    TRUE
  }
  
  # Initial load + inputs
  observeEvent(input$accdb_path, ignoreInit = TRUE, {
    if (file.exists(input$accdb_path)) {
      if (load_from_access() && !is.null(rv$missions)) {
        updateSelectInput(session, "mission_filter",
                          choices = sort(unique(rv$missions$mission)),
                          selected = head(sort(unique(rv$missions$mission)), 1))
        updateSelectizeInput(session, "missions_to_run",
                             choices = sort(unique(rv$missions$mission)), server = TRUE)
      }
    }
  })
  isolate({
    if (file.exists(default_accdb)) {
      if (load_from_access() && !is.null(rv$missions)) {
        updateSelectInput(session, "mission_filter",
                          choices = sort(unique(rv$missions$mission)),
                          selected = head(sort(unique(rv$missions$mission)), 1))
        updateSelectizeInput(session, "missions_to_run",
                             choices = sort(unique(rv$missions$mission)), server = TRUE)
      }
    }
  })
  
  # Populate the top selector whenever parent changes or the button is clicked
  observeEvent(list(input$reports_parent, input$refresh_reports), {
    choices <- build_report_choices(input$reports_parent)
    sel <- if (length(choices)) choices[[1]] else character(0)
    
    # Update the TOP selectize with client-side mode (more reliable in tabs)
    updateSelectizeInput(session, "report_picker",
                         choices  = choices,
                         selected = sel,
                         server   = FALSE)
    
    # If we have a selection, sync the single source of truth
    if (length(sel) && file.exists(sel)) {
      if (!identical(rv$selected_report(), sel)) {
        rv$selected_report(sel)
        append_log("[viewer] defaulted to: ", sel)
      }
      # Also keep LEFT inputs in sync (folder)
      updateSelectInput(session, "reports_folder", selected = dirname(sel))
    }
  }, ignoreInit = FALSE)
  
  # When the TOP selector changes, sync the LEFT pickers
  observeEvent(input$report_picker, {
    sel <- input$report_picker
    if (isTruthy(sel) && file.exists(sel)) {
      updateSelectInput(session, "reports_folder", selected = dirname(sel))
      updateSelectInput(session, "report_file",   selected = sel)
    }
  }, ignoreInit = TRUE)
  
  # When the LEFT 'Report file' changes (e.g., after profiler auto-select), mirror to TOP selector
  observeEvent(input$report_file, {
    sel <- input$report_file
    if (isTruthy(sel) && file.exists(sel)) {
      updateSelectizeInput(session, "report_picker", selected = sel)
    }
  }, ignoreInit = TRUE)
  
  # =======================
  # ▶ Loader button (external R, ODBC pre-flight, ASCII quotes)
  # =======================
  observeEvent(input$run_loader, {
    req(file.exists(input$loader_path), file.exists(input$accdb_path))
    append_log("Starting loader in external R process…")
    showNotification("Loader started (external R). See Console log.", duration = 3)
    
    loader_abs  <- normalizePath(input$loader_path, winslash = "/", mustWork = TRUE)
    scripts_dir <- dirname(loader_abs)
    accdb_abs   <- normalizePath(input$accdb_path,  winslash = "/", mustWork = TRUE)
    
    lines <- c(
      'options(warn = 1, useFancyQuotes = FALSE)',
      sprintf('setwd(%s)',    shQuote(scripts_dir, type = "cmd")),
      sprintf('accdb <- %s',  shQuote(accdb_abs,   type = "cmd")),
      sprintf('loader <- %s', shQuote(loader_abs,  type = "cmd")),
      'cat("[loader] getwd():", getwd(), "\\n")',
      
      # --- ODBC pre-flight in child ---
      'cat("[loader][odbc] Testing odbcConnectAccess2007…\\n")',
      'ok <- FALSE',
      'suppressWarnings(suppressMessages(requireNamespace("RODBC", quietly = TRUE)))',
      'con <- try(RODBC::odbcConnectAccess2007(accdb, believeNRows = FALSE), silent = TRUE)',
      'if (!inherits(con, "try-error") && !is.null(con) && !isTRUE(con < 0)) {',
      '  ok <- TRUE; cat("[loader][odbc] odbcConnectAccess2007 OK.\\n"); RODBC::odbcClose(con)',
      '} else {',
      '  cat("[loader][odbc] odbcConnectAccess2007 FAILED. Trying DriverConnect…\\n")',
      '  conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", accdb, ";Uid=;Pwd=;")',
      '  con2 <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)',
      '  if (!inherits(con2, "try-error") && !is.null(con2) && !isTRUE(con2 < 0)) {',
      '    ok <- TRUE; cat("[loader][odbc] odbcDriverConnect OK.\\n"); RODBC::odbcClose(con2)',
      '  } else {',
      '    cat("[loader][odbc][ERROR] Could not open ODBC channel to ACCDB.\\n")',
      '    msg <- try(RODBC::odbcGetErrMsg(con2), silent = TRUE)',
      '    if (!inherits(msg, "try-error")) cat("[loader][odbc] Last message: ", paste(msg, collapse=" | "), "\\n")',
      '    quit(status = 2)',
      '  }',
      '}',
      
      # --- Source the loader in child ---
      'cat("[loader] Sourcing loader…\\n")',
      'tryCatch({',
      '  source(loader, local = FALSE)',
      '  cat("[loader] Loader sourced.\\n")',
      '}, error = function(e){',
      '  cat("[loader][ERROR]", conditionMessage(e), "\\n"); quit(status = 1)',
      '})',
      
      # --- Diagnostics from loader outputs ---
      'if (exists("relcatalog_df", inherits = TRUE)) cat("[loader] relcatalog_df rows:", nrow(relcatalog_df), "\\n") else cat("[loader] relcatalog_df not found.\\n")',
      'if (exists("catalog_map_df", inherits = TRUE)) cat("[loader] catalog_map_df rows:", nrow(catalog_map_df), "\\n") else cat("[loader] catalog_map_df not found.\\n")',
      'if (exists("mp", inherits = TRUE)) cat("[loader] mapping keys:", length(mp), "\\n") else cat("[loader] mp not found.\\n")',
      'cat("[loader] cat_* objects:", length(ls()[grepl("^cat_", ls())]), "\\n")'
    )
    
    run_external_r(
      tag   = "loader",
      lines = lines,
      on_done = function() {
        # Refresh editor tables locally
        if (file.exists(input$accdb_path)) {
          if (load_from_access() && !is.null(rv$missions)) {
            updateSelectInput(session, "mission_filter",
                              choices = sort(unique(rv$missions$mission)),
                              selected = head(sort(unique(rv$missions$mission)), 1))
            updateSelectizeInput(session, "missions_to_run",
                                 choices = sort(unique(rv$missions$mission)), server = TRUE)
          }
        }
        # Optional: point viewer to latest reports (if exist)
        select_latest_report(input$reports_parent)
      }
    )
  })
  
  # =======================
  # ▶ Profiler runner (function): child setwd, ODBC pre-flight, source loader THEN profiler
  # =======================
  run_profiler_external <- function(missions = NULL) {
    req(file.exists(input$profiler_path))
    req(file.exists(input$accdb_path))
    append_log("Starting profiler in external R process (no loader in child)…")
    
    prof_abs    <- normalizePath(input$profiler_path, winslash = "/", mustWork = TRUE)
    scripts_dir <- dirname(prof_abs)
    accdb_abs   <- normalizePath(input$accdb_path,    winslash = "/", mustWork = TRUE)
    
    # RelCatalog.xlsx: use the file in the 'reports parent' folder (same one your loader uses)
    rel_xlsx_abs <- normalizePath(file.path(input$reports_parent, "RelCatalog.xlsx"),
                                  winslash = "/", mustWork = TRUE)
    
    # Mission filter line with ASCII-quoted values
    mf_line <- if (length(missions)) {
      paste0('options(somec.mission_filter = c(',
             paste(shQuote(missions, type = "cmd"), collapse = ","),
             '))')
    } else {
      'options(somec.mission_filter = NULL)'
    }
    
    # Child wrapper
    lines <- c(
      'options(warn = 1, useFancyQuotes = FALSE)',
      sprintf('setwd(%s)',    shQuote(scripts_dir, type = "cmd")),
      sprintf('accdb <- %s',  shQuote(accdb_abs,   type = "cmd")),
      sprintf('rel_xlsx <- %s', shQuote(rel_xlsx_abs, type = "cmd")),
      sprintf('prof <- %s',   shQuote(prof_abs,    type = "cmd")),
      'cat("[profiler] getwd():", getwd(), "\\n")',
      
      # --- Libraries needed in child ---
      'suppressWarnings(suppressMessages(library(RODBC)))',
      'suppressWarnings(suppressMessages(library(openxlsx)))',
      
      # --- ODBC pre-flight in child ---
      'cat("[profiler][odbc] Testing odbcConnectAccess2007…\\n")',
      'ok <- FALSE',
      'con <- try(RODBC::odbcConnectAccess2007(accdb, believeNRows = FALSE), silent = TRUE)',
      'if (!inherits(con, "try-error") && !is.null(con) && !isTRUE(con < 0)) {',
      '  ok <- TRUE; cat("[profiler][odbc] odbcConnectAccess2007 OK.\\n")',
      '} else {',
      '  cat("[profiler][odbc] odbcConnectAccess2007 FAILED. Trying DriverConnect…\\n")',
      '  conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", accdb, ";Uid=;Pwd=;")',
      '  con <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)',
      '  if (!inherits(con, "try-error") && !is.null(con) && !isTRUE(con < 0)) {',
      '    ok <- TRUE; cat("[profiler][odbc] odbcDriverConnect OK.\\n")',
      '  } else {',
      '    cat("[profiler][odbc][ERROR] Could not open ODBC channel to ACCDB.\\n")',
      '    msg <- try(RODBC::odbcGetErrMsg(con), silent = TRUE)',
      '    if (!inherits(msg, "try-error")) cat("[profiler][odbc] Last message: ", paste(msg, collapse=" | "), "\\n")',
      '    quit(status = 2)',
      '  }',
      '}',
      
      # --- Helpers to resolve Access names robustly ---
      'normalize <- function(s) tolower(gsub("\\\\s+"," ", trimws(as.character(s))))',
      'resolve_table_name <- function(candidate, all_names) {',
      '  if (candidate %in% all_names) return(candidate)',
      '  cand <- normalize(candidate); an <- normalize(all_names)',
      '  hit <- which(an == cand); if (length(hit)) return(all_names[hit[1]])',
      '  hit2 <- which(grepl(paste0("\\\\b", gsub("\\\\s+","\\\\s+", cand), "\\\\b"), an));',
      '  if (length(hit2)) return(all_names[hit2[1]]); NA_character_ }',
      
      # --- Read the three data tables directly from Access ---
      'tabs_all <- RODBC::sqlTables(con)$TABLE_NAME',
      't_miss <- resolve_table_name("missions", tabs_all)',
      't_tran <- resolve_table_name("transects", tabs_all)',
      't_obs  <- resolve_table_name("observations", tabs_all)',
      'if (is.na(t_miss) || is.na(t_tran) || is.na(t_obs)) {',
      '  cat("[profiler][ERROR] Could not resolve one or more data tables (missions/transects/observations).\\n"); quit(status=3)',
      '}',
      'missions     <- RODBC::sqlFetch(con, t_miss)',
      'transects    <- RODBC::sqlFetch(con, t_tran)',
      'observations <- RODBC::sqlFetch(con, t_obs)',
      'cat("[profiler] Read data: missions=", nrow(missions), ", transects=", nrow(transects), ", observations=", nrow(observations), "\\n", sep="")',
      
      # --- Build mapping mp from RelCatalog.xlsx + Access catalogs (no loader) ---
      'cat("[profiler] Reading RelCatalog.xlsx…\\n")',
      'rel_raw <- tryCatch(openxlsx::read.xlsx(rel_xlsx, sheet = "RelCatalog"), error=function(e) NULL)',
      'if (is.null(rel_raw) || !nrow(rel_raw)) { cat("[profiler][ERROR] RelCatalog.xlsx missing or empty.\\n"); quit(status = 4) }',
      'rn <- tolower(names(rel_raw))',
      'rel <- data.frame(',
      '  Object     = trimws(as.character(rel_raw[[which.max(rn %in% c("object","table","objet"))]])),',
      '  ColumnName = trimws(as.character(rel_raw[[which.max(rn %in% c("columnname","column","champ","field"))]])),',
      '  RefObject  = trimws(as.character(rel_raw[[which.max(rn %in% c("refobject","foreign_table","reftable","refobjet"))]])),',
      '  RefColumn  = trimws(as.character(rel_raw[[which.max(rn %in% c("refcolumn","foreigncolumn","reffield","refcolonne"))]])),',
      '  stringsAsFactors = FALSE)',
      'rel <- rel[!is.na(rel$Object) & !is.na(rel$ColumnName) & !is.na(rel$RefObject) & !is.na(rel$RefColumn), ]',
      
      'data_tables <- c("missions","transects","observations")',
      'is_data     <- tolower(rel$RefObject) %in% data_tables & !(tolower(rel$Object) %in% data_tables)',
      'rel_cat     <- rel[ is_data, , drop = FALSE ]',
      'cat("[profiler] Catalog links for data fields:", nrow(rel_cat), "\\n")',
      
      'mp <- list()',
      'for (i in seq_len(nrow(rel_cat))) {',
      '  cat_tbl <- rel_cat$Object[i];  cat_col <- rel_cat$ColumnName[i]',
      '  dat_tbl <- rel_cat$RefObject[i]; dat_col <- rel_cat$RefColumn[i]',
      '  # Resolve catalog table name in Access and fetch allowed column',
      '  t_cat <- resolve_table_name(cat_tbl, tabs_all)',
      '  if (is.na(t_cat)) { cat("[profiler][map] SKIP (catalog table not found): ", cat_tbl, "\\n", sep=""); next }',
      '  cat_df <- try(RODBC::sqlFetch(con, t_cat), silent = TRUE)',
      '  if (inherits(cat_df, "try-error") || is.null(cat_df)) { cat("[profiler][map] SKIP (cannot fetch): ", t_cat, "\\n", sep=""); next }',
      '  ccols <- names(cat_df);',
      '  # Resolve catalog column by normalized compare',
      '  cn <- normalize(ccols); want <- normalize(cat_col); hit <- which(cn == want);',
      '  if (!length(hit)) { cat("[profiler][map] SKIP (catalog column not found): ", t_cat, "$", cat_col, "\\n", sep=""); next }',
      '  allowed <- unique(na.omit(as.character(cat_df[[ccols[hit[1]]]])))',
      '  key <- paste0(tolower(dat_tbl), "$", tolower(dat_col))',
      '  mp[[key]] <- allowed',
      '  # Optional: association==999 rule could be added if needed for specific fields',
      '}',
      
      'RODBC::odbcClose(con)',
      'cat("[profiler] Mapping keys built:", length(mp), "\\n")',
      
      # --- Provide build_catalog_map() expected by your profiler; it will just return mp we built ---
      'build_catalog_map <- function() mp',
      
      # --- Apply mission filter and source profiler ---
      mf_line,
      'cat("[profiler] Missions filter:", paste(getOption("somec.mission_filter"), collapse=","), "\\n")',
      'cat("[profiler] Sourcing profiler…\\n")',
      'tryCatch({',
      '  source(prof, local = FALSE)',
      '  cat("[profiler] Profiler run completed.\\n")',
      '}, error = function(e){',
      '  cat("[profiler][ERROR] profiler:", conditionMessage(e), "\\n"); quit(status = 1)',
      '})',
      
      # --- out_dir + files listing ---
      'if (exists("cfg", inherits = TRUE)) {',
      '  base_dir <- tryCatch(cfg$base_dir, error = function(e) NA_character_)',
      '  out_dir  <- tryCatch(file.path(cfg$base_dir, paste0("MissionReports_", format(Sys.Date(), "%Y%m%d"))), error = function(e) NA_character_)',
      '  cat("[profiler] cfg$base_dir:", base_dir, "\\n")',
      '  cat("[profiler] expected out_dir:", out_dir, "\\n")',
      '  if (!is.na(out_dir)) {',
      '    if (!dir.exists(out_dir)) { dir.create(out_dir, recursive = TRUE, showWarnings = FALSE) }',
      '    f <- list.files(out_dir, pattern = "\\\\.(xlsx|xls)$", full.names = TRUE)',
      '    cat("[profiler] Files in out_dir (", out_dir, "):", if (length(f)) paste0("\\n  • ", paste(basename(f), collapse="\\n  • ")) else " <none>", "\\n", sep="")',
      '  } else {',
      '    cat("[profiler] cfg/out_dir not available in child.\\n")',
      '  }',
      '} else {',
      '  cat("[profiler] cfg not found in child.\\n")',
      '}'
    )
    
    run_external_r(
      tag   = "profiler",
      lines = lines,
      on_done = function() { select_latest_report(input$reports_parent) }
    )
  }
  
  observeEvent(input$run_profiler_sel, { run_profiler_external(input$missions_to_run) })
  observeEvent(input$run_profiler_all, { run_profiler_external(NULL) })
  
  # ------------- Reports picker + QC viewer -------------
  output$report_file_ui <- renderUI({
    req(isTruthy(input$reports_folder))
    folder <- tryCatch(normalizePath(input$reports_folder, winslash = "/", mustWork = FALSE),
                       error = function(e) input$reports_folder)
    validate(need(dir.exists(folder), paste0("Folder not found: ", folder)))
    
    files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
    validate(need(length(files) > 0, "No .xlsx files in this folder"))
    
    # Normalize to forward slashes
    files <- tryCatch(normalizePath(files, winslash = "/", mustWork = FALSE), error = function(e) files)
    
    # Put index-like files first
    idx <- grep("SOMEC_Mission_QAQC_Index_", basename(files), ignore.case = TRUE)
    files <- c(files[idx], files[-idx])
    
    # Choose selection: current rv$selected_report if it lives in this folder; otherwise first
    sel <- rv$selected_report()
    if (!isTruthy(sel) || dirname(sel) != folder || !(sel %in% files)) sel <- files[1]
    
    selectInput("report_file", "Report file:", choices = files, selected = sel)
  })
  
  
  # --- QC Summary viewer (robust & self-logging) ---
  output$qc_summary_dt <- DT::renderDT({
    # Use the single source of truth
    f <- rv$selected_report()
    req(isTruthy(f), file.exists(f))
    f <- tryCatch(normalizePath(f, winslash = "/", mustWork = FALSE), error = function(e) f)
    
    append_log(paste0("[viewer] reading: ", f))
    
    wb <- tryCatch(openxlsx::loadWorkbook(f), error = function(e) NULL)
    validate(need(!is.null(wb), paste("Cannot open workbook:", f)))
    
    sheets <- names(wb)
    append_log(paste0("[viewer] sheets: ", paste(sheets, collapse = ", ")))
    
    # Preferred sheet order: QC_Summary -> Index -> first non-empty
    pick_sheet <- function() {
      low <- tolower(sheets)
      if ("qc_summary" %in% low) return(sheets[which(low == "qc_summary")[1]])
      if ("index" %in% low)      return(sheets[which(low == "index")[1]])
      for (s in sheets) {
        df_try <- tryCatch(openxlsx::read.xlsx(f, sheet = s), error = function(e) NULL)
        if (is.data.frame(df_try) && (nrow(df_try) + ncol(df_try) > 0)) return(s)
      }
      sheets[1]
    }
    
    sheet_to_show <- pick_sheet()
    append_log(paste0("[viewer] showing sheet: ", sheet_to_show))
    
    df <- tryCatch(openxlsx::read.xlsx(f, sheet = sheet_to_show), error = function(e) NULL)
    validate(need(is.data.frame(df), paste("Cannot read sheet:", sheet_to_show)))
    
    if (nrow(df) == 0 && ncol(df) == 0) {
      return(DT::datatable(
        data.frame(note = sprintf("Sheet '%s' is empty.", sheet_to_show)),
        options = list(dom = 't', paging = FALSE, ordering = FALSE, info = FALSE),
        rownames = FALSE
      ))
    }
    
    DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  # Keep live even when the tab is hidden
  outputOptions(output, "qc_summary_dt", suspendWhenHidden = FALSE)
  
  
  
  # When TOP picker changes, set the selected report
  observeEvent(input$report_picker, {
    val <- input$report_picker
    if (isTruthy(val)) {
      val <- tryCatch(normalizePath(val, winslash = "/", mustWork = FALSE), error = function(e) val)
      if (file.exists(val) && !identical(rv$selected_report(), val)) {
        rv$selected_report(val)
        append_log("[viewer] user picked (top): ", val)
        # Keep LEFT in sync (folder selection); file dropdown will preselect this
        updateSelectInput(session, "reports_folder", selected = dirname(val))
      }
    }
  }, ignoreInit = TRUE)
  
  # When LEFT picker changes, set the selected report
  observeEvent(input$report_file, {
    val <- input$report_file
    if (isTruthy(val)) {
      val <- tryCatch(normalizePath(val, winslash = "/", mustWork = FALSE), error = function(e) val)
      if (file.exists(val) && !identical(rv$selected_report(), val)) {
        rv$selected_report(val)
        append_log("[viewer] user picked (left): ", val)
        # Keep TOP in sync
        updateSelectizeInput(session, "report_picker", selected = val, server = FALSE)
      }
    }
  }, ignoreInit = TRUE)
  
  # ------------- Filtered, editable data tables -------------
  observe({
    req(input$mission_filter, rv$missions, rv$transects, rv$observations)
    rv$missions_edit     <- rv$missions     %>% filter(.data$mission == input$mission_filter)
    rv$transects_edit    <- rv$transects    %>% filter(.data$mission == input$mission_filter)
    rv$observations_edit <- rv$observations %>% filter(.data$mission == input$mission_filter)
  })
  
  render_editable_dt <- function(id, data) {
    renderDT({
      req(data())
      datatable(data(), editable = "cell", options = list(scrollX = TRUE, pageLength = 15))
    })
  }
  
  output$missions_dt     <- render_editable_dt("missions_dt",     reactive(rv$missions_edit))
  output$transects_dt    <- render_editable_dt("transects_dt",    reactive(rv$transects_edit))
  output$observations_dt <- render_editable_dt("observations_dt", reactive(rv$observations_edit))
  
  # Cell edit handlers (store change in memory + change log)
  proxy_edit <- function(tbl_name, edit) {
    info <- edit
    i <- info$row; j <- info$col; val <- info$value
    if (tbl_name == "missions") {
      old <- rv$missions_edit[i, j, drop=TRUE]
      rv$missions_edit[i, j] <<- DT::coerceValue(val, old)
      rv$changes[[length(rv$changes)+1]] <<- list(table="missions",
                                                  keys=list(mission=rv$missions_edit$mission[i]),
                                                  column=names(rv$missions_edit)[j],
                                                  old=old, new=rv$missions_edit[i, j, drop=TRUE])
    } else if (tbl_name == "transects") {
      old <- rv$transects_edit[i, j, drop=TRUE]
      rv$transects_edit[i, j] <<- DT::coerceValue(val, old)
      key <- if ("id" %in% names(rv$transects_edit)) list(id=rv$transects_edit$id[i]) else list(mission=rv$transects_edit$mission[i])
      rv$changes[[length(rv$changes)+1]] <<- list(table="transects",
                                                  keys=key,
                                                  column=names(rv$transects_edit)[j],
                                                  old=old, new=rv$transects_edit[i, j, drop=TRUE])
    } else {
      old <- rv$observations_edit[i, j, drop=TRUE]
      rv$observations_edit[i, j] <<- DT::coerceValue(val, old)
      key <- if ("id" %in% names(rv$observations_edit)) list(id=rv$observations_edit$id[i]) else list(mission=rv$observations_edit$mission[i])
      rv$changes[[length(rv$changes)+1]] <<- list(table="observations",
                                                  keys=key,
                                                  column=names(rv$observations_edit)[j],
                                                  old=old, new=rv$observations_edit[i, j, drop=TRUE])
    }
  }
  
  observeEvent(input$missions_dt_cell_edit,     { proxy_edit("missions",     input$missions_dt_cell_edit) })
  observeEvent(input$transects_dt_cell_edit,    { proxy_edit("transects",    input$transects_dt_cell_edit) })
  observeEvent(input$observations_dt_cell_edit, { proxy_edit("observations", input$observations_dt_cell_edit) })
  
  # ------------- Save edits: write to a COPY of the Access DB -------------
  observeEvent(input$save_edits, {
    req(file.exists(input$accdb_path))
    ok_copy <- file.copy(input$accdb_path, input$accdb_copy, overwrite = TRUE)
    validate(need(ok_copy, "Could not create the copy of the ACCDB."))
    
    con <- RODBC::odbcConnectAccess2007(input$accdb_copy, believeNRows = FALSE)
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    n_applied <- 0L
    for (chg in rv$changes) {
      tbl <- chg$table
      set_vals <- setNames(list(chg$new), chg$column)
      sql <- build_update_sql(tbl, key_vals = chg$keys, set_vals = set_vals)
      status <- try(RODBC::sqlQuery(con, sql, errors = TRUE), silent = TRUE)
      if (!inherits(status, "try-error") && is.null(status)) {
        n_applied <- n_applied + 1L
      } else {
        warning("Failed SQL: ", sql)
      }
    }
    
    showNotification(paste0("Applied ", n_applied, " change(s) to: ", input$accdb_copy), type="message")
  })
  # Keep these outputs reactive even when their tab is not visible
  outputOptions(output, "console_log",   suspendWhenHidden = FALSE)
  outputOptions(output, "qc_summary_dt", suspendWhenHidden = FALSE)
  
  # Show which file is currently selected/resolved
  # ---- Selected file status (TOP then LEFT) ----
  output$qc_status <- renderText({
    sel <- rv$selected_report()
    if (!isTruthy(sel)) return("Selected: <none>")
    paste0("Selected: ", sel, if (file.exists(sel)) " (exists)" else " (not found)")
  })
  outputOptions(output, "qc_status", suspendWhenHidden = FALSE)
  
  
  # Log user selection changes explicitly (helps confirm the event fires)
  observeEvent(input$report_picker, {
    if (isTruthy(input$report_picker)) append_log("[viewer] user picked (top): ", input$report_picker)
  }, ignoreInit = TRUE)
  
  observeEvent(input$report_file, {
    if (isTruthy(input$report_file)) append_log("[viewer] user picked (left): ", input$report_file)
  }, ignoreInit = TRUE)
  
}

shinyApp(ui, server)
