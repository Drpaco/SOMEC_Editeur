# ---- app.R : Probe + Loader/Profiler + QC Viewer + floating Data Editor ----
library(shiny)
library(openxlsx)
library(callr)
library(RODBC)
library(DT)
library(xml2)
library(base64enc)
library(tools)
library(dplyr)
#----from profiler
library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)

ui <- fluidPage(
  
  # ---------- Floating Editor: CSS + Drag JS ----------
  tags$head(
    tags$style(HTML("
    .editor-float {
      position: fixed;
      top: 72px;
      left: calc(100vw - 736px);  /* 720px width + 16px margin */
      width: 720px; height: 600px;
      background: #fff;
      border: 1px solid #d0d7de;
      box-shadow: 0 12px 24px rgba(0,0,0,0.18);
      display: flex; flex-direction: column;
      resize: both;
      overflow: hidden;
      border-radius: 10px;
    }
    .editor-float-header {
      height: 46px;
      background: #0ea5e9;
      color: #fff;
      display: flex; align-items: center; justify-content: space-between;
      padding: 0 12px;
      cursor: move;
      user-select: none; -webkit-user-select: none; touch-action: none;
    }
    .editor-float-title { font-weight: 600; }
    .editor-float-controls > .btn-link {
      color: #fff; text-decoration: none; font-size: 18px; padding: 0 6px;
    }
    .editor-float-body {
      flex: 1 1 auto; overflow: auto; padding: 12px; background: #fafafa;
    }
    
    /* ---- Busy ribbon (top) ---- */
.busy-ribbon {
  position: fixed;
  top: 0; left: 0; right: 0;
  height: 4px;
  background: linear-gradient(90deg, #0ea5e9 0%, #38bdf8 50%, #0ea5e9 100%);
  background-size: 200% 100%;
  animation: ribbonSlide 1.2s linear infinite;
  display: none;
  z-index: 4000;
}
@keyframes ribbonSlide {
  0%   { background-position: 0% 50%; }
  100% { background-position: 200% 50%; }
}
/* Show while Shiny is busy */
.shiny-busy .busy-ribbon { display: block; }

    /* Layer and visibility control */
    .editor-float.front  { z-index: 2050; }           /* above app */
    .editor-float.behind { z-index: 10;   }           /* behind app (not used if hidden) */
    .editor-float.hidden { display: none !important; }/* completely hidden */
  "),
tags$style(HTML("
/* Viewer-only ribbon (shown while QC table/plot compute) */
.viewer-busy-ribbon {
  height: 4px;
  background: linear-gradient(90deg, #0ea5e9 0%, #38bdf8 50%, #0ea5e9 100%);
  background-size: 200% 100%;
  animation: ribbonSlide 1.0s linear infinite;
  margin: -6px 0 8px 0;
  border-radius: 2px;
}
"))),
    
    # Global hide/show: outside click -> hide; open button -> show; Esc -> hide
    tags$script(HTML("
    (function(){
      function getEd(){ return document.getElementById('editor_float'); }

      // Hide editor when clicking anywhere outside the editor and not the 'open_editor' button
      document.addEventListener('mousedown', function(ev){
        var ed = getEd(); if (!ed) return;
        var isOutside = !ed.contains(ev.target);
        var openBtn   = ev.target.closest && ev.target.closest('#open_editor');
        if (isOutside && !openBtn) {
          ed.classList.add('hidden');
          ed.classList.remove('front');
        }
      }, true);

      // Show editor when clicking the 'Open editor (pop-out)' button
      document.addEventListener('click', function(ev){
        var openBtn = ev.target.closest && ev.target.closest('#open_editor');
        if (openBtn) {
          var ed = getEd(); if (!ed) return;
          ed.classList.remove('hidden');
          ed.classList.add('front');
        }
      }, true);

      // Press Esc to hide the editor
      document.addEventListener('keydown', function(ev){
        if (ev.key === 'Escape') {
          var ed = getEd(); if (ed) ed.classList.add('hidden');
        }
      }, true);
    })();
  "))
  ),
  # ---------- End: Floating Editor HEAD ----------
  
  titlePanel("SOMEC — Probe + Loader/Profiler + QC Viewer + Data Editor"),
  sidebarLayout(
    sidebarPanel(
      # --- Loader / Profiler controls (unchanged) ---
      h4("Loader / Profiler"),
      textInput("loader_path",   "Loader file (.R):",   value = ""),
      textInput("profiler_path", "Profiler file (.R):", value = ""),
      
      textInput(
        "accdb_path", "Access DB (.accdb):",
        value = "C:/Users/BolducF/Documents/ShinyApps/SOMEC/BaseDeDonnees/SOMEC_20251106.accdb",  # use forward slashes
        width = "100%"
      ),
      fluidRow(
        column(6, actionButton("run_loader",   "Run Loader")),
        column(6, actionButton("run_profiler", "Run Profiler"))
      ),
      tags$hr(),
      h4("Catalog map for Viewer"),
      
      textInput(
        "relcatalog_xlsx", "RelCatalog (.xlsx):",
        value = "C:/Users/BolducF/Documents/ShinyApps/SOMEC/GestionDeDonnees/RelCatalog.xlsx",
        width = "100%"
      ),
      textInput("relcatalog_sheet", "Sheet:", value = "RelCatalog", width = "100%"),
      actionButton("load_relcatalog", "Load catalog map"),
      tags$hr()
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        # ----- TAB 1: Probe -----
        tabPanel(
          "Probe",
          h4("Console"), verbatimTextOutput("log", placeholder = TRUE),
          tags$hr(),
          h4("Sheets in selected file"), verbatimTextOutput("sheets", placeholder = TRUE)
        ),
        
        # ----- TAB 2: QC Viewer (menus on left, viewer center; editor is floating) -----
        tabPanel(
          "QC Viewer",
          fluidRow(
            # ---- LEFT: Live viewer menus (Access -> Mission -> Sheet -> Variable) ----
            column(
              width = 3,
              h4("Viewer"),
              tags$small("Uses the Access DB path from the left sidebar."),
              tags$br(),
              selectInput("mission_select", "Mission:", choices = character(0), width = "100%"),
              selectInput(
                "sheet",
                "Sheet:",
                choices = c("Missions", "Transects", "Observations"),
                selected = "Transects",
                width = "100%"
              ),
              selectInput("var_picker", "Variable:", choices = character(0), width = "100%")
            ),
            
            # ---- MIDDLE: Status, Table, Histogram ----
            column(
              width = 5,
              h4("Status"),
              textOutput("qc_status"),
              uiOutput("viewer_busy_ribbon"),
              tags$hr(),
              h4("QC table"),
              div(textOutput("var_label"), style = "margin: -6px 0 8px 0; color:#555;"),
              DTOutput("qc_table"),
              tags$hr(),
              h4("Histogram"),
              plotOutput("prof_hist", height = "260px"),
              tags$hr(),
              actionButton("open_editor", "Open editor (pop‑out)")
            )
          )
        )
      ),
      
      # ---------- Floating Editor placeholder (rendered by server when open) ----------
      uiOutput("editor_floating_panel")
      # ---------- End Floating Editor placeholder ----------
      
    )
  )
  
)

server <- function(input, output, session) {
  
  log <- reactiveVal("")
  # replace your current append_log() with this version
  append_log <- function(...) {
    line <- paste(format(Sys.time(), "%H:%M:%S"), paste(..., collapse = " "), sep = "  ")
    # Read without creating a reactive dependency
    cur  <- isolate(log())
    # Update the reactiveVal (setter is allowed)
    log(paste0(cur, if (nzchar(cur)) "\n" else "", line))
  }
  
  
  output$log <- renderText({
    invalidateLater(700, session)
    log()
  })
  
  # --- viewer busy flag + ribbon ---
  viewer_busy <- reactiveVal(FALSE)
  
  output$viewer_busy_ribbon <- renderUI({
    if (!isTRUE(viewer_busy())) return(NULL)
    div(class = "viewer-busy-ribbon")
  })
  
  # --- DEBUG BEACONS (remove when done) ---
  options(shiny.fullstacktrace = TRUE)
  
  # Fires once when the server function is constructed
  append_log("[init] server() entered")
  
  # Fires when the UI has connected (after the first flush)
  session$onFlushed(function() {
    append_log("[init] session UI bound; token=", as.character(session$token))
  }, once = TRUE)
  showNotification("SOMEC app ready.", type = "message", duration = 3)
  

  
  # --- DEBUG: proof that the RelCatalog button click is delivered ---
  observeEvent(input$load_relcatalog, {
    append_log("[catalog][probe] button click received (quick toast)")
    showNotification("RelCatalog button: click received", type = "message")
  }, ignoreInit = TRUE, priority = 1000)
  
  # --- utils ---
  `%||%` <- function(x, y) if (is.null(x)) y else x
  normalize_path <- function(p) {
    tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
  }
  supports_variable_view <- function(s) {
    if (!is.character(s) || !nzchar(s)) return(FALSE)
    tolower(s) %in% c("transects","observations")
  }
  
  catalog_allowed_for <- function(mp, table_name, col) {
    if (!is.list(mp) || !length(mp)) return(NULL)
    key <- paste0(tolower(table_name), "$", tolower(col))
    if (!key %in% names(mp)) { append_log("[catalog] no map for key: ", key); return(NULL) }
    append_log("[catalog] map HIT for key: ", key)
    mp[[key]]
  }
  
  # pick a column by case-insensitive candidates; returns the *real* column name or NULL
  pick_colname_ci <- function(df, candidates) {
    nms <- names(df); low <- tolower(nms)
    for (cand in tolower(candidates)) {
      hit <- which(low == cand)
      if (length(hit)) return(nms[hit[1]])
    }
    NULL
  }
  filter_by_mission_ci <- function(df, mission_value) {
    if (!is.data.frame(df) || !nzchar(mission_value)) return(df)
    col <- pick_colname_ci(df, c("mission","mission_id","id_mission"))
    if (is.null(col)) return(df)
    df[df[[col]] == mission_value, , drop = FALSE]
  }
  
  # ---- profiler-core helpers (local; no workbook writes) ----
  is_comment_col <- function(nm) grepl("(comment|commentaire|note|remarque)", nm, ignore.case = TRUE)
  is_dt <- function(x) inherits(x, "POSIXt") || inherits(x, "Date")
  
  clean_chr <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[x == "" | x == "NA"] <- NA_character_
    x
  }
  
  freq_table <- function(x) {
    tibble::tibble(value = as.character(x)) |>
      dplyr::mutate(value = dplyr::if_else(is.na(value) | value == "NA", NA_character_, value)) |>
      dplyr::count(value, sort = TRUE, name = "n") |>
      dplyr::mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 2))
  }
  
  plot_hist <- function(x, title="", xlab="") {
    df <- tibble::tibble(x = suppressWarnings(as.numeric(x))) |>
      dplyr::filter(is.finite(x))
    if (!nrow(df)) return(NULL)
    ggplot2::ggplot(df, ggplot2::aes(x)) +
      ggplot2::geom_histogram(color="grey30", fill="#4C78A8", bins=30) +
      ggplot2::labs(title=title, x=xlab, y="N") +
      ggplot2::theme_minimal(base_size = 10)
  }
  plot_hour_hist <- function(dt, title="") {
    v <- suppressWarnings(lubridate::as_datetime(dt))
    if (all(is.na(v))) return(NULL)
    hh <- lubridate::hour(v)
    ggplot2::ggplot(tibble::tibble(h=hh), ggplot2::aes(h)) +
      ggplot2::geom_histogram(binwidth=1, boundary=0, color="grey30", fill="#59A14F") +
      ggplot2::scale_x_continuous(breaks = 0:23) +
      ggplot2::labs(title=title, x="Heure (0–23)", y="N") +
      ggplot2::theme_minimal(base_size = 10)
  }
  
  # Build a single-variable "block": returns list(type, table_df, plot)
  prof_make_block <- function(df, col, table_name, mp = NULL, mis_start = NA, mis_end = NA) {
    col_lc <- tolower(col)
    x      <- df[[col]]
    
    # type hints
    is_datetime <- is_dt(x)
    is_charlike <- is.character(x) || is.factor(x)
    
    # catalog presence
    has_catalog <- FALSE
    if (is.list(mp)) {
      key <- paste0(tolower(table_name), "$", tolower(col))
      has_catalog <- !is.null(mp[[key]])
    }
    # forced categorical (like profiler)
    forced_cat <- col_lc %in% c("mission", "cote_obs", "code_obs")
    
    # ---- DATETIME
    if (is_datetime) {
      tbl <- tibble::tibble(
        n   = sum(!is.na(x)),
        min = as.character(suppressWarnings(min(lubridate::as_datetime(x), na.rm = TRUE))),
        max = as.character(suppressWarnings(max(lubridate::as_datetime(x), na.rm = TRUE)))
      )
      if (!any(is.na(c(mis_start, mis_end)))) {
        dt <- suppressWarnings(lubridate::as_datetime(x))
        tbl$`n_outside_mission` <- sum(dt < mis_start | dt > mis_end, na.rm = TRUE)
      }
      p <- plot_hour_hist(x, title = paste0(col, " — heure"))
      return(list(type="datetime", table_df = tbl, plot = p))
    }
    
    # ---- CATEGORICAL when:
    #   - character/factor
    #   - OR has explicit catalog
    #   - OR forced by name
    #   - OR (fallback) numeric with small cardinality (<= 25)
    small_cardinality <- is.numeric(x) && length(unique(stats::na.omit(x))) <= 25
    allowed <- catalog_allowed_for(mp, table_name, col)
    has_catalog <- !is.null(allowed)
    
    if (is_charlike || has_catalog || forced_cat || small_cardinality) {
      ft <- freq_table(x)
      ft$rare <- ft$pct < (100 * RARE_PCT)
      
      if (has_catalog) {
        ft$`found in the catalog` <- ft$value %in% allowed
        ft <- ft[, c("value", "n", "pct", "rare", "found in the catalog")]
      } else {
        ft <- ft[, c("value", "n", "pct", "rare")]
      }
      
      
      p <- if (nrow(ft)) ggplot2::ggplot(ft, ggplot2::aes(x = reorder(value, -n), y = n)) +
        ggplot2::geom_col(fill="#F28E2B") + ggplot2::coord_flip() +
        ggplot2::labs(title = paste0(col, " — fréquences"), x = NULL, y = "N") +
        ggplot2::theme_minimal(base_size = 10) else NULL
      
      return(list(type="categorical", table_df = ft, plot = p))
    }
    
    # ---- NUMERIC (fallback)
    x_num <- suppressWarnings(as.numeric(x))
    tbl <- tibble::tibble(n = sum(!is.na(x_num)), n_missing = sum(is.na(x_num)))
    p <- if (tbl$n[1] > 0) plot_hist(x_num, title = col, xlab = col) else NULL
    list(type="numeric", table_df = tbl, plot = p)
    }
  
  
  fetch_missions <- function(ap) {
    if (!isTruthy(ap) || !file.exists(ap)) return(character(0))
    con <- try(RODBC::odbcConnectAccess2007(ap, believeNRows = FALSE), silent = TRUE)
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
      conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", ap, ";Uid=;Pwd=;")
      con <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)
    }
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) return(character(0))
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    ms <- tryCatch(RODBC::sqlFetch(con, "missions"), error = function(e) NULL)
    if (!is.data.frame(ms) || !"mission" %in% names(ms)) return(character(0))
    sort(unique(as.character(ms$mission)))
  }
  
  observeEvent(input$accdb_path, {
    m <- fetch_missions(input$accdb_path)
    updateSelectInput(session, "mission_select",
                      choices = m,
                      selected = if (length(m)) m[1] else character(0))
  }, ignoreInit = FALSE)
  
  # Read current sheet for selected mission (live from Access)
  prof_sheet_df <- reactive({
    req(isTruthy(input$accdb_path), file.exists(input$accdb_path))
    mis <- input$mission_select; req(isTruthy(mis))
    sh  <- input$sheet;          req(isTruthy(sh))
    sh_l <- tolower(sh)  # "missions", "transects", or "observations"
    
    con <- try(RODBC::odbcConnectAccess2007(input$accdb_path, believeNRows = FALSE), silent = TRUE)
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
      conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", input$accdb_path, ";Uid=;Pwd=;")
      con <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)
    }
    validate(need(!inherits(con, "try-error") && !is.null(con) && !isTRUE(con < 0),
                  "Cannot open ODBC channel to ACCDB (for live profiler)."))
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    # Missions row for bounds
    ms <- tryCatch(RODBC::sqlFetch(con, "missions"), error = function(e) NULL)
    validate(need(is.data.frame(ms), "Cannot read 'missions' table."))
    mis_row <- ms %>% dplyr::filter(.data$mission == !!mis) %>% dplyr::slice_head(n = 1)
    
    # Load requested table
    # NOTE: QC_Summary may not exist in Access; this will show a clear message if not found.
    df <- tryCatch(RODBC::sqlFetch(con, sh_l), error = function(e) NULL)
    validate(need(is.data.frame(df), paste("Cannot read table from Access:", sh_l)))
    
    # Filter by mission (case-insensitive)
    n_before <- nrow(df)
    if (identical(sh_l, "missions")) {
      df <- filter_by_mission_ci(ms, mis)   # show only the selected mission row
    } else {
      df <- filter_by_mission_ci(df, mis)
    }
    
    append_log("[viewer] table=", sh_l, " mission=", mis, " rows: before=", n_before, " after=", nrow(df))    
    
    list(
      df = df,
      table_name = sh_l,
      mis_start = suppressWarnings(lubridate::as_datetime(mis_row$debut)),
      mis_end   = suppressWarnings(lubridate::as_datetime(mis_row$fin))
    )
  })
  
  observeEvent(prof_sheet_df(), {
    blk <- prof_sheet_df(); if (is.null(blk)) return()
    # Only expose variables for Transects/Observations (variable view)
    if (supports_variable_view(input$sheet)) {
      cols <- names(blk$df)
      # Optional: drop obvious technical columns
      # cols <- setdiff(cols, c("id", "id_transect", "id_obs"))
      updateSelectInput(session, "var_picker",
                        choices = cols,
                        selected = if (length(cols)) cols[1] else character(0))
    } else {
      updateSelectInput(session, "var_picker", choices = character(0), selected = character(0))
    }
  }, ignoreInit = TRUE)
  
  
  # ---- profiler config ----
  RARE_PCT <- 0.10  # 10%
  
  # ---- value-list RelCatalog (table/column/value variants, FR/EN) ----
  build_catalog_map_from_df <- function(rc_df) {
    stopifnot(is.data.frame(rc_df))
    nms <- tolower(names(rc_df)); names(rc_df) <- nms
    pick1 <- function(cands) { hit <- intersect(cands, nms); if (length(hit)) hit[1] else NA_character_ }
    
    tcol <- pick1(c("table","data_table","tbl","table_name","source_table","feuille","sheet"))
    ccol <- pick1(c("column","data_column","col","field","champ","variable"))
    vcol_code  <- pick1(c("code","val","valeur","catalogue","catalog_value","valeur_catalogue"))
    vcol_value <- pick1(c("value","allowed","level"))
    vcol_label <- pick1(c("label","libelle","libellé"))
    vcol <- if (!is.na(vcol_code)) vcol_code else if (!is.na(vcol_value)) vcol_value else vcol_label
    
    if (any(is.na(c(tcol, ccol, vcol)))) return(NULL)
    
    rc <- rc_df[, c(tcol, ccol, vcol)]
    names(rc) <- c("table","column","value")
    rc[] <- lapply(rc, function(v) trimws(as.character(v)))
    rc <- rc[ nzchar(rc$table) & nzchar(rc$column) & nzchar(rc$value), , drop = FALSE]
    if (!nrow(rc)) return(NULL)
    
    keys <- paste0(tolower(rc$table), "$", tolower(rc$column))
    mp <- lapply(split(as.character(rc$value), keys, drop = TRUE), function(v) unique(v[nzchar(v)]))
    attr(mp, "matched_columns") <- list(mode="value_list", table=tcol, column=ccol, value=vcol)
    mp
  }
  
  # ---------- Build map from relationship-style RelCatalog ----------
  # Expects headers: Object | ColumnName | RefObject | RefColumn (case-insensitive)
  build_catalog_map_from_relationships <- function(rc_df, accdb_path, progress = NULL) {
    stopifnot(is.data.frame(rc_df))
    
    # normalize headers
    nms <- tolower(names(rc_df)); names(rc_df) <- nms
    need <- c("object","columnname","refobject","refcolumn")
    if (!all(need %in% nms)) return(NULL)
    
    rc <- rc_df[, need]
    names(rc) <- c("object","column","refobject","refcolumn")
    rc[] <- lapply(rc, function(v) trimws(as.character(v)))
    rc <- unique(rc[Reduce(`&`, lapply(rc, nzchar)), , drop = FALSE])
    if (!nrow(rc)) return(NULL)
    
    con <- open_access_con(accdb_path)
    if (is.null(con)) return(NULL)
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    # Pre-scan table names once to avoid expensive sqlTables() calls in the loop
    tdf <- try(RODBC::sqlTables(con), silent = TRUE)
    if (inherits(tdf, "try-error") || is.null(tdf)) return(NULL)
    tnames <- unique(as.character(tdf$TABLE_NAME))
    tmap <- setNames(tnames, tolower(tnames))   # lower -> real
    
    # cache per RefObject
    robjs <- sort(unique(tolower(rc$refobject)))
    cache <- vector("list", length(robjs)); names(cache) <- robjs
    
    # progress by unique RefObject
    n <- length(robjs); k <- 0L
    for (ro in robjs) {
      k <- k + 1L
      if (!is.null(progress)) progress$set(value = k / max(1, n),
                                           message = sprintf("Catalog: %s (%d/%d)", ro, k, n))
      df <- sql_fetch_ci(con, ro, tmap = tmap)
      cache[[ro]] <- df
    }
    
    # build the map
    mp <- list()
    for (i in seq_len(nrow(rc))) {
      obj  <- rc$object[i]
      col  <- rc$column[i]
      robj <- tolower(rc$refobject[i])
      rcol <- rc$refcolumn[i]
      
      df <- cache[[robj]]
      if (is.null(df)) next
      vec <- pick_col_ci(df, rcol)
      if (is.null(vec)) next
      
      key  <- paste0(tolower(obj), "$", tolower(col))
      vals <- unique(as.character(vec)); vals <- vals[nzchar(vals)]
      if (!length(vals)) next
      
      if (is.null(mp[[key]])) mp[[key]] <- vals else mp[[key]] <- unique(c(mp[[key]], vals))
    }
    
    attr(mp, "matched_columns") <- list(mode = "relationships",
                                        object = "Object", column = "ColumnName",
                                        ref_object = "RefObject", ref_column = "RefColumn")
    mp
  }
  
  
  output$prof_hist <- renderPlot({
    req(supports_variable_view(input$sheet))
    viewer_busy(TRUE); on.exit(viewer_busy(FALSE), add = TRUE)
    
    withProgress(message = "Drawing histogram...", value = 0, {
      blk <- prof_sheet_df(); req(!is.null(blk)); incProgress(0.3)
      v   <- input$var_picker; req(isTruthy(v))
      df  <- blk$df
      validate(need(v %in% names(df), sprintf("Column '%s' not in %s.", v, blk$table_name)))
      mp  <- prof_map(); incProgress(0.6)
      
      one <- prof_make_block(df, v, blk$table_name, mp = mp,
                             mis_start = blk$mis_start, mis_end = blk$mis_end)
      
      incProgress(0.9)
      if (is.null(one$plot)) { plot.new(); title(main = sprintf("No data to plot for '%s'", v)) }
      else print(one$plot)
    })
  })
  
  
  # --- Data containers for Editor ---
  rv <- reactiveValues(
    missions = NULL, transects = NULL, observations = NULL,
    missions_edit = NULL, transects_edit = NULL, observations_edit = NULL,
    changes = list()
  )
  rv_qc_df <- reactiveVal(NULL)
  
  # ---------- Viewer: Status ----------
  output$qc_status <- renderText({
    ap  <- input$accdb_path %||% "<none>"
    mis <- input$mission_select %||% "<none>"
    sh  <- input$sheet %||% "<none>"
    paste0(
      "ACCDB: ", if (isTruthy(ap) && file.exists(ap)) ap else "<none>",
      "  | Mission: ", mis,
      "  | Sheet: ", sh
    )
  })
  
  output$catalog_status <- renderText({
    mp <- prof_map()
    sprintf("Catalog map fields: %d", if (is.list(mp)) length(mp) else 0L)
  })
  
  rv_map <- reactiveVal(NULL)
  
  prof_map <- reactive({
    # 1) Explicit cache (preferred)
    mp <- rv_map()
    if (is.list(mp) && length(mp)) return(mp)
    
    # 2) Fallback to Loader's function if it exists (no button)
    if (exists("build_catalog_map")) {
      mp2 <- try(build_catalog_map(), silent = TRUE)
      if (!inherits(mp2, "try-error") && is.list(mp2)) return(mp2)
    }
    # 3) Empty map
    list()
  })
  
  # ---------- Robust Access helpers ----------
  # ---------- Robust Access helpers ----------
  open_access_con <- function(ap) {
    con <- try(RODBC::odbcConnectAccess2007(ap, believeNRows = FALSE), silent = TRUE)
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
      conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", ap, ";Uid=;Pwd=;")
      con <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)
    }
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) return(NULL)
    con
  }
  
  # Case-insensitive table fetch; also tries 'cat_' prefix
  sql_fetch_ci <- function(con, tbl, tmap = NULL) {
    # Use pre-scanned table map if provided
    if (!is.null(tmap)) {
      real <- tmap[[tolower(tbl)]]
      if (!is.null(real)) return(tryCatch(RODBC::sqlFetch(con, real), error = function(e) NULL))
      # try 'cat_' prefix
      real <- tmap[[paste0("cat_", tolower(tbl))]]
      if (!is.null(real)) return(tryCatch(RODBC::sqlFetch(con, real), error = function(e) NULL))
      return(NULL)
    }
    all_tbls <- try(RODBC::sqlTables(con), silent = TRUE)
    if (inherits(all_tbls, "try-error") || is.null(all_tbls)) return(NULL)
    tnames <- unique(as.character(all_tbls$TABLE_NAME))
    hit <- tnames[tolower(tnames) == tolower(tbl)]
    if (!length(hit)) hit <- tnames[tolower(tnames) == paste0("cat_", tolower(tbl))]
    if (!length(hit)) return(NULL)
    tryCatch(RODBC::sqlFetch(con, hit[1]), error = function(e) NULL)
  }
  
  # Case-insensitive column accessor
  pick_col_ci <- function(df, col) {
    if (!is.data.frame(df)) return(NULL)
    hit <- which(tolower(names(df)) == tolower(col))
    if (!length(hit)) return(NULL)
    df[[ hit[1] ]]
  }
  
  observeEvent(input$load_relcatalog, {
    # --- Immediate visual feedback so you know the click is received ---
    notif_id <- showNotification("Loading RelCatalog…", type = "message", duration = NULL, closeButton = TRUE)
    on.exit({ try(removeNotification(notif_id), silent = TRUE) }, add = TRUE)
    
    append_log("[catalog] load_relcatalog clicked")
    
    # --- Self-contained helpers (no external deps required) ---
    open_access_con_local <- function(ap) {
      con <- try(RODBC::odbcConnectAccess2007(ap, believeNRows = FALSE), silent = TRUE)
      if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
        conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", ap, ";Uid=;Pwd=;")
        con <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)
      }
      if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) return(NULL)
      con
    }
    sql_fetch_ci_local <- function(con, tbl, tmap) {
      # Use the pre-scanned map of lower->real table names
      real <- tmap[[tolower(tbl)]]
      if (!is.null(real)) return(tryCatch(RODBC::sqlFetch(con, real), error = function(e) NULL))
      # try cat_ prefix
      real <- tmap[[paste0("cat_", tolower(tbl))]]
      if (!is.null(real)) return(tryCatch(RODBC::sqlFetch(con, real), error = function(e) NULL))
      NULL
    }
    pick_col_ci_local <- function(df, col) {
      if (!is.data.frame(df)) return(NULL)
      hit <- which(tolower(names(df)) == tolower(col))
      if (!length(hit)) return(NULL)
      df[[ hit[1] ]]
    }
    
    tryCatch({
      # Prefer loader function if present
      if (exists("build_catalog_map")) {
        append_log("[catalog] trying Loader build_catalog_map()")
        mp <- try(build_catalog_map(), silent = TRUE)
        if (!inherits(mp, "try-error") && is.list(mp) && length(mp)) {
          rv_map(mp)
          append_log("[catalog] map loaded via Loader: ", length(mp), " field(s)")
          showNotification(sprintf("Catalog map loaded from Loader (%d fields).", length(mp)), type = "message")
          return(invisible())
        }
        append_log("[catalog] Loader map unavailable; moving to RelCatalog.xlsx")
      }
      
      # Read RelCatalog.xlsx
      path  <- input$relcatalog_xlsx
      sheet <- input$relcatalog_sheet %||% "RelCatalog"
      if (!isTruthy(path) || !file.exists(path)) {
        append_log("[catalog][ERROR] RelCatalog path not found: ", path)
        showNotification("RelCatalog path is empty or not found.", type = "error")
        return(invisible())
      }
      rc <- try(openxlsx::read.xlsx(path, sheet = sheet), silent = TRUE)
      if (inherits(rc, "try-error") || !is.data.frame(rc) || !nrow(rc)) {
        append_log("[catalog][ERROR] Cannot read RelCatalog sheet: ", sheet)
        showNotification(sprintf("Cannot read RelCatalog sheet: %s", sheet), type = "error")
        return(invisible())
      }
      
      nms <- tolower(names(rc))
      # Your relationships schema
      if (!all(c("object","columnname","refobject","refcolumn") %in% nms)) {
        append_log("[catalog][ERROR] RelCatalog sheet is not relationships schema (needs Object/ColumnName/RefObject/RefColumn)")
        showNotification("RelCatalog sheet must have Object/ColumnName/RefObject/RefColumn.", type = "error")
        return(invisible())
      }
      
      # Open ACCDB once
      con <- open_access_con_local(input$accdb_path)
      if (is.null(con)) {
        append_log("[catalog][ERROR] Cannot open ACCDB to resolve relationships.")
        showNotification("Cannot open ACCDB to resolve relationships.", type = "error")
        return(invisible())
      }
      on.exit(RODBC::odbcClose(con), add = TRUE)
      
      # Pre-scan table names once
      tdf <- try(RODBC::sqlTables(con), silent = TRUE)
      if (inherits(tdf, "try-error") || is.null(tdf)) {
        append_log("[catalog][ERROR] ACCDB sqlTables() failed.")
        showNotification("ACCDB metadata read failed.", type = "error")
        return(invisible())
      }
      tnames <- unique(as.character(tdf$TABLE_NAME))
      tmap   <- setNames(tnames, tolower(tnames))
      
      # Normalize RelCatalog; drop empties; cache each RefObject once
      rc2 <- rc
      names(rc2) <- tolower(names(rc2))
      rc2[] <- lapply(rc2, function(v) trimws(as.character(v)))
      rc2 <- unique(rc2[Reduce(`&`, lapply(rc2, nzchar)), , drop = FALSE])
      robjs <- sort(unique(tolower(rc2$refobject)))
      
      # Progress over unique RefObjects
      mp <- list()
      withProgress(message = "Resolving catalog relationships…", value = 0, {
        n <- length(robjs)
        cache <- vector("list", n); names(cache) <- robjs
        
        for (k in seq_along(robjs)) {
          ro <- robjs[k]
          incProgress(k / max(1, n), detail = sprintf("Reading %s…", ro))
          cache[[ro]] <- sql_fetch_ci_local(con, ro, tmap = tmap)
        }
        
        # Build the map
        for (i in seq_len(nrow(rc2))) {
          obj  <- rc2$object[i]
          col  <- rc2$columnname[i]
          robj <- tolower(rc2$refobject[i])
          rcol <- rc2$refcolumn[i]
          
          df <- cache[[robj]]
          if (is.null(df)) next
          vec <- pick_col_ci_local(df, rcol)
          if (is.null(vec)) next
          
          key  <- paste0(tolower(obj), "$", tolower(col))
          vals <- unique(as.character(vec)); vals <- vals[nzchar(vals)]
          if (!length(vals)) next
          
          if (is.null(mp[[key]])) mp[[key]] <- vals else mp[[key]] <- unique(c(mp[[key]], vals))
        }
      })
      
      if (!length(mp)) {
        append_log("[catalog][warn] Relationship map resolved to 0 field(s).")
        showNotification("RelCatalog resolved to 0 fields.", type = "warning")
        return(invisible())
      }
      
      rv_map(mp)
      append_log("[catalog] map built from relationships: ", length(mp), " field(s)")
      showNotification(sprintf("Catalog map loaded (%d fields).", length(mp)), type = "message")
      
    }, error = function(e) {
      append_log("[catalog][ERROR] ", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)

  
  
  output$var_label <- renderText({
    if (!supports_variable_view(input$sheet)) return("")
    v <- input$var_picker
    if (!isTruthy(v)) return("Variable: <none>")
    paste0("Variable: ", v)
  })
  
  
  output$qc_table <- DT::renderDT({
    viewer_busy(TRUE); on.exit(viewer_busy(FALSE), add = TRUE)
    
    withProgress(message = "Building table...", value = 0, {
      blk <- prof_sheet_df(); req(!is.null(blk)); incProgress(0.2)
      sh  <- input$sheet;      req(isTruthy(sh))
      df  <- blk$df
      
      if (supports_variable_view(sh)) {
        v <- input$var_picker; req(isTruthy(v))
        validate(need(v %in% names(df), sprintf("Column '%s' not found in %s.", v, blk$table_name)))
        mp  <- prof_map(); incProgress(0.6)
        
        one <- prof_make_block(df, v, blk$table_name, mp = mp,
                               mis_start = blk$mis_start, mis_end = blk$mis_end)
        tbl <- one$table_df
        validate(need(is.data.frame(tbl), "No rows to display for this variable."))
        
        rv_qc_df(tbl); incProgress(0.9)
        
        return(DT::datatable(
          tbl,
          options = list(
            dom = if (nrow(tbl) > 15) "tip" else "t",
            pageLength = min(15, max(5, nrow(tbl))),
            scrollX = TRUE,
            ordering = FALSE
          ),
          selection = "single",
          rownames  = FALSE
        ))
      }
      
      # Full-sheet mode for Missions
      validate(need(is.data.frame(df), paste("Cannot display table for:", blk$table_name)))
      rv_qc_df(df); incProgress(0.9)
      
      DT::datatable(
        df,
        options = list(pageLength = 15, scrollX = TRUE),
        selection = "single",
        rownames  = FALSE
      )
    })
  })
  
  

  
  # ---------- Data Editor ----------
  # Load from Access DB (sidebar version; optional)
  load_from_access <- function() {
    ap <- input$accdb_path
    if (!isTruthy(ap) || !file.exists(ap)) {
      append_log("[access] ACCDB not found: ", ap); return(invisible(FALSE))
    }
    append_log("[access] Opening: ", ap)
    con <- try(RODBC::odbcConnectAccess2007(ap, believeNRows = FALSE), silent = TRUE)
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
      append_log("[access] odbcConnectAccess2007 failed. Trying DriverConnect…")
      conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", ap, ";Uid=;Pwd=;")
      con <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)
      if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
        append_log("[access][ERROR] Cannot open ODBC channel to ACCDB.")
        return(invisible(FALSE))
      }
    }
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    rv$missions     <- tryCatch(RODBC::sqlFetch(con, "missions"),     error = function(e) NULL)
    rv$transects    <- tryCatch(RODBC::sqlFetch(con, "transects"),    error = function(e) NULL)
    rv$observations <- tryCatch(RODBC::sqlFetch(con, "observations"), error = function(e) NULL)
    if (is.null(rv$missions) || is.null(rv$transects) || is.null(rv$observations)) {
      append_log("[access][ERROR] One or more base tables not found.")
      return(invisible(FALSE))
    }
    append_log("[access] Loaded: missions=", nrow(rv$missions),
               ", transects=", nrow(rv$transects),
               ", observations=", nrow(rv$observations))
    
    # Mission filter options (legacy; not used in floating editor)
    m_uniq <- sort(unique(as.character(rv$missions$mission)))
    updateSelectInput(session, "mission_filter", choices = m_uniq,
                      selected = if (length(m_uniq)) m_uniq[1] else character(0))
    
    # Default COPY path next to ACCDB
    if (!nzchar(input$accdb_copy) && file.exists(ap)) {
      updateTextInput(session, "accdb_copy",
                      value = file.path(dirname(ap), "SOMEC_EDIT_COPY.accdb"))
    }
    TRUE
  }
  
  observeEvent(input$load_accdb, {
    ok <- load_from_access()
    if (isTRUE(ok)) append_log("[ui] ACCDB loaded.")
  })
  
  # ---- OPEN/CLOSE STATE ----
  editor_open <- reactiveVal(FALSE)
  
  # Button in the Viewer ("Open editor (pop-out)")
  observeEvent(input$open_editor, {
    editor_open(TRUE)
    # Prefill panel ACCDB path from the sidebar
    ap <- input$accdb_path
    if (isTruthy(ap)) {
      updateTextInput(session, "accdb_path_modal", value = ap)
      if (file.exists(ap)) {
        updateTextInput(session, "accdb_copy_modal",
                        value = file.path(dirname(ap), "SOMEC_EDIT_COPY.accdb"))
      }
    }
  })
  
  # ---- FLOATING EDITOR BODY (builder)----
  editor_floating_body <- function() {
    tagList(
      fluidRow(
        column(
          width = 5,
          h4("Editor controls"),
          helpText("Set the Access DB path here or in the left sidebar. Paths stay in sync."),
          textInput("accdb_path_modal", "Access DB (.accdb):", value = "", width = "100%"),
          fluidRow(
            column(6, actionButton("load_accdb_modal", "Load ACCDB")),
            column(6, textInput("accdb_copy_modal", "Save to COPY (.accdb):",
                                value = "", width = "100%"))
          ),
          tags$small("Tip: pick a COPY path above before saving."),
          tags$hr(),
          selectInput("mission_filter_modal", "Mission to edit:", choices = character(0)),
          tags$hr(),
          actionButton("save_edits_modal", "Save edits to COPY")
        ),
        column(
          width = 7,
          tabsetPanel(
            id = "editor_tabs_modal",
            tabPanel("Missions",     DTOutput("missions_dt_modal")),
            tabPanel("Transects",    DTOutput("transects_dt_modal")),
            tabPanel("Observations", DTOutput("observations_dt_modal"))
          )
        )
      )
    )
  }
  
  # ---- FLOATING EDITOR SHELL ----
  output$editor_floating_panel <- renderUI({
    if (!isTRUE(editor_open())) return(NULL)
    
    panel <- tags$div(
      id    = "editor_float",
      class = "editor-float front",         # <— add 'front' here
      style = "top:72px; left: calc(100vw - 736px);",
      tags$div(
        id = "editor_float_header",
        class = "editor-float-header",
        tags$span(class = "editor-float-title", "SOMEC — Data Editor"),
        tags$div(class = "editor-float-controls",
                 actionLink("close_editor", label = "\u00D7", class = "btn-link", title = "Close")
        )
      ),
      tags$div(class = "editor-float-body", editor_floating_body())
    )
    
    # Inline, robust drag binder: runs after the panel is in the DOM.
    binder <- tags$script(HTML("
    (function() {
      var el = document.getElementById('editor_float');
      var hd = document.getElementById('editor_float_header');
      if (!el || !hd) return;

      // Only bind once
      if (el.dataset.dragBound === '1') return;
      el.dataset.dragBound = '1';

      // Ensure we only use left/top (never right)
      el.style.right = 'auto';
      if (!el.style.left)  el.style.left  = (window.innerWidth - 736) + 'px';
      if (!el.style.top)   el.style.top   = '72px';

      var startX=0, startY=0, startLeft=0, startTop=0, moving=false;

      function onDown(e) {
        e.preventDefault();
        
        el.classList.remove('hidden');
        el.classList.add('front');

        var rect = el.getBoundingClientRect();
        startLeft = rect.left;
        startTop  = rect.top;
        startX    = e.clientX;
        startY    = e.clientY;
        moving    = true;
        document.addEventListener('mousemove', onMove, true);
        document.addEventListener('mouseup', onUp, true);
      }
      function onMove(e) {
        if (!moving) return;
        e.preventDefault();
        var dx = e.clientX - startX;
        var dy = e.clientY - startY;
        el.style.left = (startLeft + dx) + 'px';
        el.style.top  = (startTop  + dy) + 'px';
      }
      function onUp() {
        moving = false;
        document.removeEventListener('mousemove', onMove, true);
        document.removeEventListener('mouseup', onUp, true);
      }

      // Use capture so nested elements inside header don't block the mousedown
      hd.addEventListener('mousedown', onDown, true);
      // Touch support (optional)
      hd.addEventListener('touchstart', function(ev){
        if (!ev.touches || !ev.touches.length) return;
        var t = ev.touches[0];
        var rect = el.getBoundingClientRect();
        startLeft = rect.left; startTop = rect.top;
        startX = t.clientX; startY = t.clientY; moving = true;
        function tMove(evt){
          var tt = evt.touches[0]; if (!tt) return;
          var dx = tt.clientX - startX, dy = tt.clientY - startY;
          el.style.left = (startLeft + dx) + 'px';
          el.style.top  = (startTop  + dy) + 'px';
        }
        function tEnd(){
          moving = false;
          document.removeEventListener('touchmove', tMove, true);
          document.removeEventListener('touchend', tEnd, true);
        }
        document.addEventListener('touchmove', tMove, true);
        document.addEventListener('touchend', tEnd, true);
      }, {passive:false, capture:true});
    })();
  "))
    
    # Return panel + inline binder
    tagList(panel, binder)
  })
  
  # "× Close" button inside the floating panel
  observeEvent(input$close_editor, {
    editor_open(FALSE)
  })
  
  # Sidebar -> Panel
  observeEvent(input$accdb_path, {
    if (isTruthy(input$accdb_path) &&
        !identical(input$accdb_path, input$accdb_path_modal)) {
      updateTextInput(session, "accdb_path_modal", value = input$accdb_path)
    }
  }, ignoreInit = TRUE)
  
  # Panel -> Sidebar
  observeEvent(input$accdb_path_modal, {
    if (isTruthy(input$accdb_path_modal) &&
        !identical(input$accdb_path_modal, input$accdb_path)) {
      updateTextInput(session, "accdb_path", value = input$accdb_path_modal)
    }
  }, ignoreInit = TRUE)
  
  # Helper that loads 3 base tables from a given path
  load_from_access_path <- function(ap) {
    if (!isTruthy(ap) || !file.exists(ap)) {
      append_log("[access] ACCDB not found: ", ap); return(invisible(FALSE))
    }
    append_log("[access] Opening: ", ap)
    con <- try(RODBC::odbcConnectAccess2007(ap, believeNRows = FALSE), silent = TRUE)
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
      append_log("[access] odbcConnectAccess2007 failed. Trying DriverConnect…")
      conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", ap, ";Uid=;Pwd=;")
      con <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)
      if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
        append_log("[access][ERROR] Cannot open ODBC channel to ACCDB.")
        return(invisible(FALSE))
      }
    }
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    rv$missions     <- tryCatch(RODBC::sqlFetch(con, "missions"),     error = function(e) NULL)
    rv$transects    <- tryCatch(RODBC::sqlFetch(con, "transects"),    error = function(e) NULL)
    rv$observations <- tryCatch(RODBC::sqlFetch(con, "observations"), error = function(e) NULL)
    if (is.null(rv$missions) || is.null(rv$transects) || is.null(rv$observations)) {
      append_log("[access][ERROR] One or more base tables not found.")
      return(invisible(FALSE))
    }
    append_log("[access] Loaded: missions=", nrow(rv$missions),
               ", transects=", nrow(rv$transects),
               ", observations=", nrow(rv$observations))
    
    # Fill the panel mission filter
    m_uniq <- sort(unique(as.character(rv$missions$mission)))
    updateSelectInput(session, "mission_filter_modal", choices = m_uniq,
                      selected = if (length(m_uniq)) m_uniq[1] else character(0))
    TRUE
  }
  
  # Panel "Load ACCDB"
  observeEvent(input$load_accdb_modal, {
    ap <- input$accdb_path_modal %||% input$accdb_path
    ok <- load_from_access_path(ap)
    if (isTRUE(ok)) append_log("[ui][panel] ACCDB loaded.")
  })
  
  # Keep modal editor slices in sync with modal filter
  observe({
    req(input$mission_filter_modal, rv$missions, rv$transects, rv$observations)
    rv$missions_edit     <- rv$missions     %>% filter(.data$mission == input$mission_filter_modal)
    rv$transects_edit    <- rv$transects    %>% filter(.data$mission == input$mission_filter_modal)
    rv$observations_edit <- rv$observations %>% filter(.data$mission == input$mission_filter_modal)
  })
  
  rv_hi <- reactiveValues(
    missions = integer(0),
    transects = integer(0),
    observations = integer(0)
  )
  
  render_editable_dt_hi <- function(data_react, hi_key) {
    renderDT({
      df <- data_react(); req(is.data.frame(df))
      hi_idx <- rv_hi[[hi_key]] %||% integer(0)     # 1-based
      hi_js  <- if (length(hi_idx)) paste0(hi_idx - 1L, collapse = ",") else ""
      row_cb <- DT::JS(
        "function(row, data, displayNum, displayIndex, dataIndex) {",
        sprintf("  var hi = [%s];", hi_js),
        "  if (hi.indexOf(dataIndex) !== -1) {",
        "    $(row).css({'background-color':'#fff7b1'});",
        "  }",
        "}"
      )
      datatable(
        df, editable = "cell",
        selection = "single", rownames = FALSE,
        options   = list(scrollX = TRUE, pageLength = 10, rowCallback = row_cb)
      )
    })
  }
  
  # Use the highlighting renderer for the panel tables
  output$missions_dt_modal     <- render_editable_dt_hi(reactive(rv$missions_edit),     "missions")
  output$transects_dt_modal    <- render_editable_dt_hi(reactive(rv$transects_edit),    "transects")
  output$observations_dt_modal <- render_editable_dt_hi(reactive(rv$observations_edit), "observations")
  
  # Reuse your proxy_edit handlers for _modal tables:
  observeEvent(input$missions_dt_modal_cell_edit,     { proxy_edit("missions",     input$missions_dt_modal_cell_edit) })
  observeEvent(input$transects_dt_modal_cell_edit,    { proxy_edit("transects",    input$transects_dt_modal_cell_edit) })
  observeEvent(input$observations_dt_modal_cell_edit, { proxy_edit("observations", input$observations_dt_modal_cell_edit) })
  
  observeEvent(input$mission_filter_modal, {
    rv_hi$missions <- rv_hi$transects <- rv_hi$observations <- integer(0)
  }, ignoreInit = TRUE)
  
  observeEvent(input$qc_table_rows_selected, {
    sel <- input$qc_table_rows_selected
    df  <- rv_qc_df()
    if (!length(sel) || is.null(df)) return()
    
    clicked <- df[sel[1], , drop = FALSE]
    
    # Try to infer 'mission' from common column names
    nms <- tolower(names(clicked))
    mission_col <- intersect(nms, c("mission", "mission_id", "id_mission"))
    mission_val <- if (length(mission_col)) as.character(clicked[[ mission_col[1] ]]) else NULL
    
    if (isTruthy(mission_val)) {
      # Update the panel mission filter (if ACCDB is loaded)
      if (!is.null(rv$missions)) {
        choices <- sort(unique(as.character(rv$missions$mission)))
        updateSelectInput(session, "mission_filter_modal",
                          choices = choices,
                          selected = if (mission_val %in% choices) mission_val else choices[1])
      }
      # Compute highlight indices on filtered edit dfs
      if (!is.null(rv$missions_edit)) {
        rv_hi$missions <- which(as.character(rv$missions_edit$mission) == mission_val)
      }
      if (!is.null(rv$transects_edit)) {
        rv_hi$transects <- which(as.character(rv$transects_edit$mission) == mission_val)
      }
      if (!is.null(rv$observations_edit)) {
        rv_hi$observations <- which(as.character(rv$observations_edit$mission) == mission_val)
      }
      append_log("[viewer->editor] mission='", mission_val, "'; hi rows — M:", length(rv_hi$missions),
                 " T:", length(rv_hi$transects), " O:", length(rv_hi$observations))
    } else {
      rv_hi$missions <- rv_hi$transects <- rv_hi$observations <- integer(0)
      append_log("[viewer->editor] No mission column found in clicked row; highlights cleared.")
    }
  })
  
  # (Legacy sidebar editor pieces kept for compatibility; safe to remove later)
  observe({
    req(input$mission_filter, rv$missions, rv$transects, rv$observations)
    rv$missions_edit     <- rv$missions     %>% filter(.data$mission == input$mission_filter)
    rv$transects_edit    <- rv$transects    %>% filter(.data$mission == input$mission_filter)
    rv$observations_edit <- rv$observations %>% filter(.data$mission == input$mission_filter)
  })
  
  render_editable_dt <- function(data) {
    renderDT({
      req(data())
      datatable(data(), editable = "cell", options = list(scrollX = TRUE, pageLength = 10))
    })
  }
  output$missions_dt     <- render_editable_dt(reactive(rv$missions_edit))
  output$transects_dt    <- render_editable_dt(reactive(rv$transects_edit))
  output$observations_dt <- render_editable_dt(reactive(rv$observations_edit))
  
  # Track edits
  proxy_edit <- function(tbl_name, edit) {
    info <- edit; i <- info$row; j <- info$col; val <- info$value
    if (tbl_name == "missions") {
      old <- rv$missions_edit[i, j, drop=TRUE]
      rv$missions_edit[i, j] <<- DT::coerceValue(val, old)
      rv$changes[[length(rv$changes)+1]] <<- list(
        table="missions",
        keys=list(mission=rv$missions_edit$mission[i]),
        column=names(rv$missions_edit)[j],
        old=old, new=rv$missions_edit[i, j, drop=TRUE]
      )
    } else if (tbl_name == "transects") {
      old <- rv$transects_edit[i, j, drop=TRUE]
      rv$transects_edit[i, j] <<- DT::coerceValue(val, old)
      key <- if ("id" %in% names(rv$transects_edit)) list(id=rv$transects_edit$id[i])
      else list(mission=rv$transects_edit$mission[i])
      rv$changes[[length(rv$changes)+1]] <<- list(
        table="transects",
        keys=key,
        column=names(rv$transects_edit)[j],
        old=old, new=rv$transects_edit[i, j, drop=TRUE]
      )
    } else {
      old <- rv$observations_edit[i, j, drop=TRUE]
      rv$observations_edit[i, j] <<- DT::coerceValue(val, old)
      key <- if ("id" %in% names(rv$observations_edit)) list(id=rv$observations_edit$id[i])
      else list(mission=rv$observations_edit$mission[i])
      rv$changes[[length(rv$changes)+1]] <<- list(
        table="observations",
        keys=key,
        column=names(rv$observations_edit)[j],
        old=old, new=rv$observations_edit[i, j, drop=TRUE]
      )
    }
  }
  observeEvent(input$missions_dt_cell_edit,     { proxy_edit("missions",     input$missions_dt_cell_edit) })
  observeEvent(input$transects_dt_cell_edit,    { proxy_edit("transects",    input$transects_dt_cell_edit) })
  observeEvent(input$observations_dt_cell_edit, { proxy_edit("observations", input$observations_dt_cell_edit) })
  
  # --- Helpers (only define once in your server) ---
  sql_literal <- function(x) {
    if (is.na(x)) return("NULL")
    if (inherits(x, "Date")) return(paste0("#", format(x, "%Y-%m-%d"), "#"))
    if (is.numeric(x)) return(as.character(x))
    paste0("'", gsub("'", "''", as.character(x)), "'")
  }
  build_update_sql <- function(tbl, key_vals, set_vals) {
    set_clauses   <- paste0("[", names(set_vals), "] = ", vapply(set_vals, sql_literal, character(1)))
    where_clauses <- paste0("[", names(key_vals), "] = ", vapply(key_vals, sql_literal, character(1)))
    paste0("UPDATE [", tbl, "] SET ", paste(set_clauses, collapse = ", "),
           " WHERE ", paste(where_clauses, collapse = " AND "), ";")
  }
  
  # Save edits from floating panel
  observeEvent(input$save_edits_modal, {
    ap <- input$accdb_path_modal %||% input$accdb_path
    cp <- input$accdb_copy_modal
    
    req(length(rv$changes) > 0, isTruthy(ap), file.exists(ap), isTruthy(cp))
    
    ok_copy <- try(file.copy(ap, cp, overwrite = TRUE), silent = TRUE)
    if (!isTRUE(ok_copy)) {
      showNotification("Could not create the copy of the ACCDB.", type="error")
      append_log("[save][ERROR] Copy failed: ", ap, " -> ", cp)
      return()
    }
    
    con <- try(RODBC::odbcConnectAccess2007(cp, believeNRows = FALSE), silent = TRUE)
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
      conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", cp, ";Uid=;Pwd=;")
      con <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)
      if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
        showNotification("ODBC open failed on COPY.", type="error")
        append_log("[save][ERROR] ODBC open failed on COPY: ", cp)
        return()
      }
    }
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    n_applied <- 0L; n_failed <- 0L
    for (chg in rv$changes) {
      set_vals <- setNames(list(chg$new), chg$column)
      sql <- build_update_sql(chg$table, key_vals = chg$keys, set_vals = set_vals)
      status <- try(RODBC::sqlQuery(con, sql, errors = TRUE), silent = TRUE)
      if (!inherits(status, "try-error") && is.null(status)) n_applied <- n_applied + 1L
      else { n_failed <- n_failed + 1L; append_log("[save][WARN] failed SQL: ", sql) }
    }
    
    showNotification(paste0("Applied ", n_applied, " change(s)",
                            if (n_failed) paste0(" (", n_failed, " failed)") else "",
                            " to: ", cp),
                     type = if (n_failed) "warning" else "message")
    append_log("[save] Applied ", n_applied, " change(s)",
               if (n_failed) paste0(" (", n_failed, " failed)") else "", " to: ", cp)
    
    rv$changes <- list()
  })
  
  # ========== External process runner (Loader/Profiler) ==========
  rv_log_index <- reactiveVal(list())
  read_new_log_lines <- function(logf) {
    if (!file.exists(logf)) return(character(0))
    all  <- readLines(logf, warn = FALSE)
    prev <- rv_log_index()[[logf]] %||% 0L
    if (length(all) <= prev) return(character(0))
    new  <- all[(prev + 1L):length(all)]
    idx  <- rv_log_index(); idx[[logf]] <- length(all); rv_log_index(idx)
    new
  }
  
  run_external_r <- function(tag, script_path, accdb_path = "") {
    req(isTruthy(script_path), file.exists(script_path))
    script_abs  <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
    scripts_dir <- dirname(script_abs)
    
    lines <- c(
      'options(warn = 1, useFancyQuotes = FALSE)',
      sprintf('setwd(%s)',    shQuote(scripts_dir, type = "cmd")),
      sprintf('script_file <- %s', shQuote(script_abs, type = "cmd")),
      'cat("[child][info] getwd():", getwd(), "\\n")'
    )
    if (nzchar(accdb_path) && file.exists(accdb_path)) {
      accdb_abs <- normalizePath(accdb_path, winslash = "/", mustWork = TRUE)
      lines <- c(lines,
                 sprintf('accdb <- %s', shQuote(accdb_abs, type = "cmd")),
                 'cat("[child][odbc] Testing odbcConnectAccess2007…\\n")',
                 'suppressWarnings(suppressMessages(library(RODBC)))',
                 'ok <- FALSE',
                 'con <- try(RODBC::odbcConnectAccess2007(accdb, believeNRows = FALSE), silent = TRUE)',
                 'if (!inherits(con, "try-error") && !is.null(con) && !isTRUE(con < 0)) {',
                 '  ok <- TRUE; cat("[child][odbc] odbcConnectAccess2007 OK.\\n"); RODBC::odbcClose(con)',
                 '} else {',
                 '  cat("[child][odbc] odbcConnectAccess2007 FAILED. Trying DriverConnect…\\n")',
                 '  conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", accdb, ";Uid=;Pwd=;")',
                 '  con2 <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)',
                 '  if (!inherits(con2, "try-error") && !is.null(con2) && !isTRUE(con2 < 0)) {',
                 '    ok <- TRUE; cat("[child][odbc] odbcDriverConnect OK.\\n"); RODBC::odbcClose(con2)',
                 '  } else {',
                 '    cat("[child][odbc][ERROR] Could not open ODBC channel to ACCDB.\\n")',
                 '  }',
                 '}'
      )
    } else {
      lines <- c(lines, 'cat("[child][odbc] ACCDB not provided; skipping ODBC pre-flight.\\n")')
    }
    lines <- c(lines,
               'cat("[child] Sourcing script…\\n")',
               'tryCatch({ source(script_file, local = FALSE); cat("[child] Script finished.\\n") },',
               '  error = function(e){ cat("[child][ERROR] ", conditionMessage(e), "\\n", sep="") })'
    )
    
    wrapper <- tempfile(fileext = ".R"); writeLines(lines, wrapper)
    logf <- tempfile(fileext = ".log"); idx <- rv_log_index(); idx[[logf]] <- 0L; rv_log_index(idx)
    append_log(sprintf("[%s] wrapper: %s | log: %s", tag, wrapper, logf))
    
    p <- callr::r_bg(function(wrap) { source(wrap, local = FALSE) },
                     args = list(wrap = wrapper), stdout = logf, stderr = logf)
    
    obs <- NULL
    obs <- observe({
      invalidateLater(1000, session)
      out <- read_new_log_lines(logf); if (length(out)) append_log(paste(out, collapse = "\n"))
      if (!p$is_alive()) {
        out <- read_new_log_lines(logf); if (length(out)) append_log(paste(out, collapse = "\n"))
        append_log(sprintf("[%s] finished. Exit status: %s", tag, p$get_exit_status()))
        showNotification(sprintf("%s finished.", paste0(toupper(substr(tag, 1, 1)), substr(tag, 2, 99))), type = "message")
        if (!is.null(obs)) obs$destroy()
      }
    })
  }
  observeEvent(input$run_loader,   { req(isTruthy(input$loader_path),   file.exists(input$loader_path));   append_log("[ui] Run Loader clicked.");   run_external_r("loader",   input$loader_path,   input$accdb_path) })
  observeEvent(input$run_profiler, { req(isTruthy(input$profiler_path), file.exists(input$profiler_path)); append_log("[ui] Run Profiler clicked."); run_external_r("profiler", input$profiler_path, input$accdb_path) })
}

shinyApp(ui, server)