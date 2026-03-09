# ============================================================
# app.R — SOMEC Interactive QA & Editor (Baseline Rollback)
# - Starts reliably
# - Console log shows activity
# - Scans MissionReports_* and lists .xlsx files
# - Populates Sheet dropdown and renders tables (with raw fallback)
# - Variable-by-variable and Images are disabled for now
# ============================================================

library(shiny)
library(DT)
library(openxlsx)
library(RODBC)
library(dplyr)
library(callr)

# ----------------------- DEFAULT PATHS -----------------------
default_loader   <- "C:/Users/BolducF/Documents/ShinyApps/SOMEC_Editeur/catalog_loader.R"
default_profiler <- "C:/Users/BolducF/Documents/ShinyApps/SOMEC_Editeur/mission_profiler_somerc_v6_3_4.R"
default_accdb    <- "C:/Users/BolducF/Documents/ShinyApps/SOMEC/BaseDeDonnees/SOMEC_20251106.accdb"
default_reports  <- "C:/Users/BolducF/Documents/ShinyApps/SOMEC/GestionDeDonnees/"
# ------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .tab-content { min-height: 360px; }
      pre#console_log { max-height: 45vh; overflow: auto; }
    ")),
    tags$script(HTML("
      $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function () {
        $('.dataTable').each(function(){
          try { $(this).DataTable().columns.adjust(); } catch(e) {}
        });
      });
    "))
  ),
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
        id = "main_tabs",
        tabPanel(
          "Console log",
          div(style = "padding:8px; white-space:pre-wrap;",
              verbatimTextOutput("console_log", placeholder = TRUE))
        ),
        tabPanel(
          "QC Summary viewer",
          
          # Top: file picker + Refresh
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
              div(style = "margin-top:26px;",
                  actionButton("refresh_reports", "Refresh list"))
            )
          ),
          
          tags$hr(),
          
          # Status
          div(textOutput("qc_status"),
              style = "margin-bottom:8px; color:#555; font-size:12px;"),
          
          # Sheet (only)
          fluidRow(
            column(6, selectInput("sheet", "Sheet:", choices = NULL))
          ),
          
          tags$hr(),
          
          h5("QC table"),
          DTOutput("qc_table")
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

server <- function(input, output, session) {
  
  # ===== Reactive containers & log =====
  rv <- reactiveValues(
    missions = NULL, transects = NULL, observations = NULL,
    missions_edit = NULL, transects_edit = NULL, observations_edit = NULL,
    changes = list()
  )
  rv$log <- reactiveVal("")
  rv$selected_report <- reactiveVal(NULL)
  
  append_log <- function(...) {
    txt <- paste(format(Sys.time(), "%H:%M:%S"), paste(..., collapse = " "), sep = "  ")
    current <- rv$log()
    keep <- if (nzchar(current)) substr(current, max(1, nchar(current) - 10000), nchar(current)) else ""
    rv$log(paste0(keep, if (nzchar(keep)) "\n" else "", txt))
  }
  output$console_log <- renderText({
    invalidateLater(500, session)  # force redraw twice per second
    rv$log()
  })
  outputOptions(output, "console_log", suspendWhenHidden = FALSE)
  
  # ✅ One-shot startup log in a *reactive* context (safe)
  observeEvent(TRUE, {
    append_log("[init] main app started")
  }, once = TRUE, ignoreInit = FALSE)
  
  # Log immediately so you see the app loop is alive
#  session$onFlushed(function() { append_log("[init] app started — parent=", input$reports_parent) }, once = TRUE)
  
  # ===== Helpers =====
  norm_path <- function(p) tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
  
  # Run scan *once* on startup (reactive-safe)
  observeEvent(TRUE, {
    do_refresh_reports()
  }, once = TRUE, ignoreInit = FALSE)
  
  # Run scan when parent changes or Refresh is clicked
  observeEvent(list(input$reports_parent, input$refresh_reports), {
    do_refresh_reports()
  }, ignoreInit = FALSE)
  
  # One source of truth for the selected file everywhere
  current_report_file <- reactive({
    f <- rv$selected_report()
    if (!isTruthy(f) || !file.exists(f)) {
      f <- input$report_picker
      if (!isTruthy(f) || !file.exists(f)) f <- input$report_file
    }
    if (isTruthy(f) && file.exists(f)) norm_path(f) else NULL
  })
  
  # ===== Access loader =====
  load_from_access <- function() {
    ap <- input$accdb_path
    if (!isTruthy(ap) || !file.exists(ap)) {
      append_log("[access] ACCDB not found: ", ap); return(invisible(FALSE))
    }
    append_log("[access] Opening: ", ap)
    con <- try(RODBC::odbcConnectAccess2007(ap, believeNRows = FALSE), silent = TRUE)
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
      append_log("[access] odbcConnectAccess2007 failed. Check driver/bitness/path.")
      return(invisible(FALSE))
    }
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    try({ rv$missions     <- sqlFetch(con, "missions") })
    try({ rv$transects    <- sqlFetch(con, "transects") })
    try({ rv$observations <- sqlFetch(con, "observations") })
    append_log("[access] Data frames refreshed.")
    TRUE
  }
  
  # Initial data load (if default exists)
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
  
  # ===== Reports scanning =====
  build_report_choices <- function(parent) {
    parent <- tryCatch(normalizePath(parent, winslash = "/", mustWork = FALSE), error = function(e) parent)
    if (!dir.exists(parent)) return(character(0))
    
    subs <- list.dirs(parent, full.names = TRUE, recursive = FALSE)
    subs <- subs[grepl("MissionReports_[0-9]{8}$", basename(subs), ignore.case = TRUE)]
    if (!length(subs)) return(character(0))
    subs <- subs[order(basename(subs), decreasing = TRUE)]
    
    files <- unlist(
      lapply(subs, function(d) list.files(d, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)),
      use.names = FALSE
    )
    if (!length(files)) return(character(0))
    files <- tryCatch(normalizePath(files, winslash = "/", mustWork = FALSE), error = function(e) files)
    
    # Index first
    idx <- grep("SOMEC_Mission_QAQC_Index_", basename(files), ignore.case = TRUE)
    files <- c(files[idx], files[-idx])
    
    labs <- paste0(basename(dirname(files)), " • ", basename(files))
    stats::setNames(files, labs)
  }
  
  do_refresh_reports <- function() {
    parent <- tryCatch(normalizePath(input$reports_parent, winslash = "/", mustWork = FALSE),
                       error = function(e) input$reports_parent)
    append_log("[refresh] parent=", parent, " exists=", dir.exists(parent))
    
    choices <- build_report_choices(parent)
    append_log("[refresh] choices: ", length(choices))
    
    sel <- if (length(choices)) choices[[1]] else character(0)
    updateSelectizeInput(session, "report_picker", choices = choices, selected = sel, server = FALSE)
    
    if (length(sel) && file.exists(sel)) {
      rv$selected_report(sel)
      append_log("[viewer] defaulted to: ", sel)
      updateSelectInput(session, "reports_folder", selected = dirname(sel))
    } else {
      updateSelectInput(session, "reports_folder", choices = character(0))
      updateSelectInput(session, "report_file",   choices = character(0))
      showNotification("No MissionReports_* or .xlsx files found under the parent folder.", type="warning", duration=5)
    }
  }
  
  # Run scan on start and when parent / Refresh clicked
  observeEvent(TRUE, { do_refresh_reports() }, once = TRUE, ignoreInit = FALSE)
  observeEvent(list(input$reports_parent, input$refresh_reports), { do_refresh_reports() }, ignoreInit = FALSE)
  
  # Left pickers (short labels)
  output$reports_folder_ui <- renderUI({
    parent <- input$reports_parent
    validate(need(isTruthy(parent) && is.character(parent) && length(parent) == 1, "Set a valid parent folder."))
    parent <- tryCatch(normalizePath(parent, winslash = "/", mustWork = FALSE), error = function(e) parent)
    validate(need(dir.exists(parent), paste0("Folder not found: ", parent)))
    
    subs <- list.dirs(parent, full.names = TRUE, recursive = FALSE)
    keep <- subs[grepl("MissionReports_[0-9]{8}$", basename(subs), ignore.case = TRUE)]
    if (!length(keep)) return(div(paste("No MissionReports_YYYYMMDD under:", parent)))
    
    keep <- keep[order(basename(keep), decreasing = TRUE)]
    labs <- stats::setNames(keep, basename(keep))
    selectInput("reports_folder", "Reports folder:", choices = labs)
  })
  output$report_file_ui <- renderUI({
    req(isTruthy(input$reports_folder))
    folder <- tryCatch(normalizePath(input$reports_folder, winslash = "/", mustWork = FALSE),
                       error = function(e) input$reports_folder)
    validate(need(dir.exists(folder), paste0("Folder not found: ", folder)))
    
    files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
    validate(need(length(files) > 0, "No .xlsx files in this folder"))
    
    files <- tryCatch(normalizePath(files, winslash = "/", mustWork = FALSE), error = function(e) files)
    idx <- grep("SOMEC_Mission_QAQC_Index_", basename(files), ignore.case = TRUE)
    files <- c(files[idx], files[-idx])
    
    labs <- stats::setNames(files, basename(files))
    selectInput("report_file", "Report file:", choices = labs)
  })
  
  observeEvent(input$reports_parent, {
    append_log("[init] reports_parent = ", input$reports_parent)
  }, ignoreInit = FALSE)
  
  # Keep top and left in sync
  observeEvent(input$report_picker, {
    sel <- input$report_picker
    if (isTruthy(sel) && file.exists(sel)) {
      rv$selected_report(sel)
      append_log("[viewer] user picked (top): ", sel)
      updateSelectInput(session, "reports_folder", selected = dirname(sel))
      updateSelectInput(session, "report_file",   selected = sel)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$report_file, {
    sel <- input$report_file
    if (isTruthy(sel) && file.exists(sel)) {
      rv$selected_report(sel)
      append_log("[viewer] user picked (left): ", sel)
      updateSelectizeInput(session, "report_picker", selected = sel, server = FALSE)
    }
  }, ignoreInit = TRUE)
  
  # ===== Status line =====
  output$qc_status <- renderText({
    sel <- rv$selected_report()
    if (!isTruthy(sel)) return("Selected: <none>")
    paste0("Selected: ", sel, if (file.exists(sel)) " (exists)" else " (not found)")
  })
  outputOptions(output, "qc_status", suspendWhenHidden = FALSE)
  
  # ===== SHEETS: populate whenever selected_report changes =====
  observeEvent(rv$selected_report(), {
    f <- rv$selected_report(); req(file.exists(f))
    wb <- tryCatch(openxlsx::loadWorkbook(f), error = function(e) NULL); req(!is.null(wb))
    sheets <- names(wb); req(length(sheets))
    
    low <- tolower(sheets)
    pick_first <- function(cand) { hit <- which(low == cand); if (length(hit)) sheets[hit[1]] else NA_character_ }
    pref <- c(pick_first("transects"), pick_first("observations"),
              pick_first("qc_summary"), pick_first("missions"),
              pick_first("index"), sheets[1])
    pref <- pref[!is.na(pref)][1]
    
    updateSelectInput(session, "sheet", choices = sheets, selected = pref)
    append_log("[viewer] sheet choices set; selected: ", pref)
  }, ignoreInit = FALSE)
  
  # Safety: ensure Sheet is never blank
  observe({
    f <- rv$selected_report()
    if (!isTruthy(f) || !file.exists(f)) return()
    if (!isTruthy(input$sheet) || !nzchar(input$sheet)) {
      sn <- tryCatch(openxlsx::getSheetNames(f), error = function(e) NULL)
      if (length(sn)) {
        updateSelectInput(session, "sheet", choices = sn, selected = sn[1])
        append_log("[viewer] sheet was empty -> forced to first: ", sn[1])
      }
    }
  })
  
  # ===== TABLE (with raw fallback) =====
  output$qc_table <- DT::renderDT({
    f <- rv$selected_report(); s <- input$sheet
    req(isTruthy(f), file.exists(f), isTruthy(s))
    
    append_log("[table] render sheet: ", s)
    
    df <- tryCatch(
      openxlsx::read.xlsx(
        f, sheet = s, colNames = TRUE, detectDates = TRUE,
        skipEmptyRows = FALSE, skipEmptyCols = FALSE
      ),
      error = function(e) NULL
    )
    
    if (is.null(df) || (nrow(df) == 0 && ncol(df) == 0)) {
      append_log("[table] header read empty; fallback to raw for: ", s)
      df <- tryCatch(
        openxlsx::read.xlsx(
          f, sheet = s, colNames = FALSE, detectDates = TRUE,
          skipEmptyRows = FALSE, skipEmptyCols = FALSE
        ),
        error = function(e) NULL
      )
      validate(need(is.data.frame(df), paste("Cannot read sheet:", s)))
      if (ncol(df) > 0) names(df) <- paste0("C", seq_len(ncol(df)))
      return(DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE))
    }
    
    fix_names <- function(x) { x <- as.character(x); x[is.na(x) | x == ""] <- "X"; make.unique(x, sep = "_") }
    names(df) <- fix_names(names(df))
    
    DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  outputOptions(output, "qc_table", suspendWhenHidden = FALSE)
  
  # ===== Data editor =====
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
                                                  keys=key, column=names(rv$transects_edit)[j],
                                                  old=old, new=rv$transects_edit[i, j, drop=TRUE])
    } else {
      old <- rv$observations_edit[i, j, drop=TRUE]
      rv$observations_edit[i, j] <<- DT::coerceValue(val, old)
      key <- if ("id" %in% names(rv$observations_edit)) list(id=rv$observations_edit$id[i]) else list(mission=rv$observations_edit$mission[i])
      rv$changes[[length(rv$changes)+1]] <<- list(table="observations",
                                                  keys=key, column=names(rv$observations_edit)[j],
                                                  old=old, new=rv$observations_edit[i, j, drop=TRUE])
    }
  }
  observeEvent(input$missions_dt_cell_edit,     { proxy_edit("missions",     input$missions_dt_cell_edit) })
  observeEvent(input$transects_dt_cell_edit,    { proxy_edit("transects",    input$transects_dt_cell_edit) })
  observeEvent(input$observations_dt_cell_edit, { proxy_edit("observations", input$observations_dt_cell_edit) })
  
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
      set_clause <- paste0("[", names(set_vals), "] = ", if (is.numeric(chg$new)) chg$new else paste0("'", gsub("'", "''", as.character(chg$new)), "'"))
      where_clause <- paste0("[", names(chg$keys), "] = ", if (is.numeric(chg$keys[[1]])) chg$keys[[1]] else paste0("'", gsub("'", "''", as.character(chg$keys[[1]])), "'"))
      sql <- paste0("UPDATE [", tbl, "] SET ", set_clause, " WHERE ", where_clause, ";")
      status <- try(RODBC::sqlQuery(con, sql, errors = TRUE), silent = TRUE)
      if (!inherits(status, "try-error") && is.null(status)) n_applied <- n_applied + 1L
    }
    showNotification(paste0("Applied ", n_applied, " change(s) to: ", input$accdb_copy), type="message")
  })
}

shinyApp(ui, server)
