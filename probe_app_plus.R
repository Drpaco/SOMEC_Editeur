# ---- app.R : Probe + Loader/Profiler + QC Viewer + side-by-side Data Editor ----
library(shiny)
library(openxlsx)
library(callr)
library(RODBC)
library(DT)
library(xml2)
library(base64enc)
library(tools)
library(dplyr)

ui <- fluidPage(
  titlePanel("SOMEC — Probe + Loader/Profiler + QC Viewer + Data Editor"),
  
  sidebarLayout(
    sidebarPanel(
      # --- Loader / Profiler controls (unchanged) ---
      h4("Loader / Profiler"),
      textInput("loader_path",   "Loader file (.R):",   value = ""),
      textInput("profiler_path", "Profiler file (.R):", value = ""),
      textInput("accdb_path",    "Access DB (.accdb):", value = ""),  # used by Editor + child runners
      fluidRow(
        column(6, actionButton("run_loader",   "Run Loader")),
        column(6, actionButton("run_profiler", "Run Profiler"))
      ),
      tags$hr()
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        # ----- TAB 1: Probe -----
        tabPanel(
          "Probe",
          # shared report menus moved to Viewer; Probe reads current selections
          h4("Console"), verbatimTextOutput("log", placeholder = TRUE),
          tags$hr(),
          h4("Sheets in selected file"), verbatimTextOutput("sheets", placeholder = TRUE)
        ),
        
        # ----- TAB 2: QC Viewer + Data Editor side-by-side -----
        tabPanel(
          "QC Viewer",
          fluidRow(
            # ---- LEFT: All report / viewer menus ----
            column(
              width = 3,
              h4("Reports"),
              textInput("parent", "Reports parent folder:",
                        value = "C:/Users/BolducF/Documents/ShinyApps/SOMEC/GestionDeDonnees",
                        width = "100%"),
              actionButton("refresh", "Refresh list"),
              tags$hr(),
              selectInput("folder", "Reports folder:", choices = character(0)),
              selectInput("file",   "Report file (.xlsx):", choices = character(0)),
              tags$hr(),
              h4("Viewer"),
              selectInput("sheet", "Sheet:", choices = character(0)),
              conditionalPanel(
                condition = "['transects','observations'].includes((input.sheet || '').toLowerCase())",
                selectInput("var_picker", "Variable:", choices = character(0))
              ),
              tags$hr(),
              actionButton("debug_dump", "Debug dump")
            ),
            
            # ---- MIDDLE: Status, Table, Images ----
            column(
              width = 5,
              h4("Status"),
              textOutput("qc_status"),
              tags$hr(),
              h4("QC table"),
              div(textOutput("var_label"), style = "margin: -6px 0 8px 0; color:#555;"),
              DTOutput("qc_table"),
              tags$hr(),
              h4("Embedded images"),
              uiOutput("img_gallery")
            ),
            
            # ---- RIGHT: Data Editor ----
            column(
              width = 4,
              h4("Data editor"),
              helpText("Set ACCDB path in the left sidebar above (Access DB)."),
              fluidRow(
                column(6, actionButton("load_accdb", "Load ACCDB")),
                column(6, textInput("accdb_copy", "Save to COPY (.accdb):",
                                    value = "", width = "100%"))
              ),
              tags$small("Tip: pick a COPY path above before saving."),
              tags$hr(),
              selectInput("mission_filter", "Mission to edit:", choices = character(0)),
              tabsetPanel(
                id = "editor_tabs",
                tabPanel("Missions",     DTOutput("missions_dt")),
                tabPanel("Transects",    DTOutput("transects_dt")),
                tabPanel("Observations", DTOutput("observations_dt"))
              ),
              tags$hr(),
              actionButton("save_edits", "Save edits to COPY")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # --- simple in-app console ---
  log <- reactiveVal("")
  append_log <- function(...) {
    line <- paste(format(Sys.time(), "%H:%M:%S"), paste(..., collapse = " "), sep = "  ")
    cur  <- log()
    log(paste0(cur, if (nzchar(cur)) "\n" else "", line))
  }
  output$log <- renderText({ invalidateLater(500, session); log() })
  
  # --- utils ---
  `%||%` <- function(x, y) if (is.null(x)) y else x
  normalize_path <- function(p) {
    tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
  }
  supports_variable_view <- function(s) {
    if (!is.character(s) || !nzchar(s)) return(FALSE)
    tolower(s) %in% c("transects","observations")
  }
  
  # --- Data containers for Editor ---
  rv <- reactiveValues(
    missions = NULL, transects = NULL, observations = NULL,
    missions_edit = NULL, transects_edit = NULL, observations_edit = NULL,
    changes = list()
  )
  
  # --- Probe scanner (unchanged logic) ---
  scan_reports <- function(parent) {
    parent <- tryCatch(normalizePath(parent, winslash = "/", mustWork = FALSE), error = function(e) parent)
    append_log("[scan] parent=", parent, " exists=", dir.exists(parent))
    
    subs <- if (dir.exists(parent)) list.dirs(parent, full.names = TRUE, recursive = FALSE) else character(0)
    mrs  <- subs[grepl("MissionReports_[0-9]{8}$", basename(subs), perl = TRUE)]
    append_log("[scan] MR folders: ", length(mrs),
               if (length(mrs)) paste0(" [", paste(basename(mrs), collapse = ", "), "]") else "")
    list(parent = parent, mrs = mrs)
  }
  observeEvent(TRUE, { append_log("[init] app started") }, once = TRUE, ignoreInit = FALSE)
  
  observeEvent(list(input$parent, input$refresh), {
    r <- scan_reports(input$parent)
    if (!length(r$mrs)) {
      updateSelectInput(session, "folder", choices = character(0))
      updateSelectInput(session, "file",   choices = character(0))
      updateSelectInput(session, "sheet",  choices = character(0))
      updateSelectInput(session, "var_picker", choices = character(0))
      append_log("[scan] no MissionReports_*")
      return()
    }
    # newest first
    pick <- r$mrs[order(basename(r$mrs), decreasing = TRUE)]
    updateSelectInput(session, "folder", choices = setNames(pick, basename(pick)), selected = pick[1])
    
    files <- list.files(pick[1], pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
    files <- tryCatch(normalizePath(files, winslash = "/", mustWork = FALSE), error = function(e) files)
    append_log("[scan] .xlsx in ", basename(pick[1]), ": ", length(files),
               if (length(files)) paste0(" [", paste(basename(head(files, 8)), collapse = ", "),
                                         if (length(files) > 8) ", ..." else "", "]") else "")
    updateSelectInput(session, "file",
                      choices  = setNames(files, basename(files)),
                      selected = if (length(files)) files[1] else character(0))
  }, ignoreInit = FALSE)
  
  observeEvent(input$file, {
    f <- input$file
    if (!isTruthy(f) || !file.exists(f)) {
      updateSelectInput(session, "sheet",      choices = character(0))
      updateSelectInput(session, "var_picker", choices = character(0))
      output$sheets <- renderText("<none>")
      append_log("[ui] invalid file")
      return()
    }
    f <- normalize_path(f)
    sh <- tryCatch(openxlsx::getSheetNames(f), error = function(e) NULL)
    output$sheets <- renderText(if (length(sh)) paste(sh, collapse = ", ") else "<error/null>")
    append_log("[workbook] sheets: ", if (length(sh)) paste(sh, collapse = ", ") else "<error/null>")
    
    if (!length(sh)) {
      updateSelectInput(session, "sheet",      choices = character(0))
      updateSelectInput(session, "var_picker", choices = character(0))
      return()
    }
    
    low <- tolower(sh)
    pick_first <- function(cand) { hit <- which(low == cand); if (length(hit)) sh[hit[1]] else NA_character_ }
    pref <- c(pick_first("transects"), pick_first("observations"),
              pick_first("qc_summary"), pick_first("missions"),
              pick_first("index"), sh[1])
    pref <- pref[!is.na(pref)][1]
    
    updateSelectInput(session, "sheet", choices = sh, selected = pref)
    append_log("[ui] sheet selected: ", pref)
  }, ignoreInit = TRUE)
  
  # ---------- Viewer: Status / Variables / Table / Images ----------
  output$qc_status <- renderText({
    f <- input$file
    if (!isTruthy(f)) return("Selected: <none>")
    f <- normalize_path(f)
    paste0("Selected: ", f, if (file.exists(f)) " (exists)" else " (not found)")
  })
  
  detect_variable_titles <- function(xlsx, sheet_name) {
    raw <- tryCatch(openxlsx::read.xlsx(xlsx, sheet = sheet_name, colNames = FALSE,
                                        detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE),
                    error = function(e) NULL)
    if (is.null(raw) || !nrow(raw)) return(data.frame(var = character(0), row_start = integer(0)))
    titles <- list()
    for (r in seq_len(nrow(raw))) {
      row_vals <- raw[r, , drop = TRUE]
      hit <- which(vapply(as.character(row_vals), function(cell) {
        if (is.na(cell)) return(FALSE)
        grepl("^\\s*[•\\-]\\s*.+\\(", cell)   # bullet or dash + "name ("
      }, logical(1)))
      if (length(hit)) {
        first_cell <- as.character(row_vals[hit[1]])
        v <- sub("^\\s*[•\\-]\\s*", "", first_cell)
        v <- sub("\\s*\\(.*$", "", v)
        v <- trimws(v)
        if (nzchar(v)) titles[[length(titles) + 1L]] <- data.frame(var = v, row_start = r)
      }
    }
    if (!length(titles)) return(data.frame(var = character(0), row_start = integer(0)))
    do.call(rbind, titles)
  }
  
  sheet_index_of <- function(file, sheet_name) {
    sn <- tryCatch(openxlsx::getSheetNames(file), error = function(e) NULL)
    if (is.null(sn)) return(NA_integer_)
    hit <- which(sn == sheet_name)
    if (length(hit)) hit[1] else NA_integer_
  }
  resolve_target <- function(tmp_root, base_dir, target) {
    if (grepl("^/", target)) {
      return(normalizePath(file.path(tmpdir, sub("^/", "", target)), winslash = "/", mustWork = FALSE))
    }
    normalizePath(file.path(base_dir, target), winslash = "/", mustWork = FALSE)
  }
  extract_images_with_anchors <- function(xlsx, sheet_idx) {
    out <- data.frame(img_path = character(0), row = integer(0), col = integer(0), stringsAsFactors = FALSE)
    if (is.na(sheet_idx) || sheet_idx <= 0) return(out)
    tmpdir <- tempfile("unz_"); dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
    utils::unzip(xlsx, exdir = tmpdir)
    
    sheet_rels <- file.path(tmpdir, "xl", "worksheets", "_rels", sprintf("sheet%d.xml.rels", sheet_idx))
    if (!file.exists(sheet_rels)) return(out)
    
    rels <- tryCatch(xml2::read_xml(sheet_rels), error = function(e) NULL); if (is.null(rels)) return(out)
    rel_nodes <- xml2::xml_find_all(rels, ".//*[local-name()='Relationship']")
    if (!length(rel_nodes)) return(out)
    rType   <- xml2::xml_attr(rel_nodes, "Type")
    rTarget <- xml2::xml_attr(rel_nodes, "Target")
    draw_targets <- rTarget[
      grepl("officeDocument/2006/relationships/drawing", rType, fixed = TRUE) |
        grepl("drawings/", rTarget, fixed = TRUE)
    ]
    if (!length(draw_targets)) return(out)
    
    all_rows <- list()
    for (t in draw_targets) {
      drawing_xml <- normalizePath(file.path(dirname(sheet_rels), "..", sub("^.*/", "", t)),
                                   winslash = "/", mustWork = FALSE)
      if (!grepl("xl/drawings/", drawing_xml, fixed = TRUE)) {
        maybe <- normalizePath(file.path(dirname(dirname(sheet_rels)), "drawings", basename(drawing_xml)),
                               winslash = "/", mustWork = FALSE)
        if (file.exists(maybe)) drawing_xml <- maybe
      }
      drawing_rels_xml <- sub("xl/drawings/drawing([0-9]+)\\.xml$",
                              "xl/drawings/_rels/drawing\\1.xml.rels", drawing_xml)
      if (!file.exists(drawing_rels_xml)) next
      
      d_rels <- tryCatch(xml2::read_xml(drawing_rels_xml), error = function(e) NULL); if (is.null(d_rels)) next
      d_nodes <- xml2::xml_find_all(d_rels, ".//*[local-name()='Relationship']")
      d_rId   <- xml2::xml_attr(d_nodes, "Id")
      d_tgt   <- xml2::xml_attr(d_nodes, "Target")
      id_to_img <- setNames(
        vapply(d_tgt, function(it) {
          imgp <- normalizePath(file.path(dirname(drawing_rels_xml), "..", it),
                                winslash = "/", mustWork = FALSE)
          if (!grepl("xl/media/", imgp, fixed = TRUE)) {
            maybe <- normalizePath(file.path(dirname(dirname(drawing_rels_xml)), "media", basename(imgp)),
                                   winslash = "/", mustWork = FALSE)
            if (file.exists(maybe)) imgp <- maybe
          }
          imgp
        }, character(1)),
        d_rId
      )
      
      dxml <- tryCatch(xml2::read_xml(drawing_xml), error = function(e) NULL); if (is.null(dxml)) next
      anchors <- c(xml2::xml_find_all(dxml, ".//*[local-name()='twoCellAnchor']"),
                   xml2::xml_find_all(dxml, ".//*[local-name()='oneCellAnchor']"))
      if (!length(anchors)) next
      
      for (a in anchors) {
        fromNode <- xml2::xml_find_first(a, ".//*[local-name()='from']")
        r <- suppressWarnings(as.integer(xml2::xml_text(xml2::xml_find_first(fromNode, ".//*[local-name()='row']"))))
        c <- suppressWarnings(as.integer(xml2::xml_text(xml2::xml_find_first(fromNode, ".//*[local-name()='col']"))))
        r <- if (is.na(r)) NA_integer_ else r + 1L
        c <- if (is.na(c)) NA_integer_ else c + 1L
        
        blip <- xml2::xml_find_first(a, ".//*[local-name()='blip']")
        rid  <- xml2::xml_attr(blip, "r:embed"); if (is.na(rid) || is.null(rid)) rid <- xml2::xml_attr(blip, "embed")
        imgp <- if (!is.null(rid) && rid %in% names(id_to_img)) id_to_img[[rid]] else NA_character_
        
        if (!is.na(imgp) && file.exists(imgp) && !is.na(r) && !is.na(c)) {
          all_rows[[length(all_rows) + 1L]] <- data.frame(img_path = imgp, row = r, col = c, stringsAsFactors = FALSE)
        }
      }
    }
    if (!length(all_rows)) return(out)
    do.call(rbind, all_rows)
  }
  
  rv_img_index <- reactiveVal(NULL)
  observeEvent(list(input$file, input$sheet), {
    f <- input$file; s <- input$sheet
    if (!isTruthy(f) || !file.exists(f) || !isTruthy(s)) {
      rv_img_index(NULL); updateSelectInput(session, "var_picker", choices = character(0)); return()
    }
    f <- normalize_path(f)
    idx <- sheet_index_of(f, s); append_log("[images] sheet index: ", idx)
    
    by_sheet <- tryCatch(extract_images_with_anchors(f, idx), error = function(e) NULL)
    vars <- if (supports_variable_view(s)) detect_variable_titles(f, s) else data.frame(var=character(0), row_start=integer(0))
    if (nrow(vars)) append_log("[vars] detected: ", paste(vars$var, collapse=" | "))
    
    if (is.null(by_sheet) || !nrow(by_sheet)) {
      append_log("[images] no anchors; fallback to /xl/media")
      imgs_all <- tryCatch(extract_all_images(f), error=function(e) character(0))
      rv_img_index(list(
        by_sheet = if (length(imgs_all)) data.frame(img_path = imgs_all, row = NA_integer_, col = NA_integer_) else data.frame(img_path=character(0),row=integer(0),col=integer(0)),
        by_var   = data.frame(var=character(0), img_path=character(0), row=integer(0), col=integer(0)),
        vars     = vars
      ))
    } else {
      by_var <- data.frame(var=character(0), img_path=character(0), row=integer(0), col=integer(0))
      if (nrow(vars)) {
        vars <- vars[order(vars$row_start), ]
        for (i in seq_len(nrow(by_sheet))) {
          r_img <- by_sheet$row[i]
          k <- max(which(vars$row_start <= r_img))
          if (length(k) && is.finite(k)) {
            by_var <- rbind(by_var, data.frame(
              var = vars$var[k], img_path = by_sheet$img_path[i], row = by_sheet$row[i], col = by_sheet$col[i],
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      rv_img_index(list(by_sheet = by_sheet, by_var = by_var, vars = vars))
    }
    
    if (supports_variable_view(s) && nrow(vars)) {
      updateSelectInput(session, "var_picker", choices = vars$var, selected = vars$var[1])
    } else {
      updateSelectInput(session, "var_picker", choices = character(0))
    }
  }, ignoreInit = TRUE)
  
  output$var_label <- renderText({
    if (!supports_variable_view(input$sheet)) return("")
    v <- input$var_picker
    if (!isTruthy(v)) return("Variable: <none>")
    paste0("Variable: ", v)
  })
  
  output$qc_table <- DT::renderDT({
    f <- input$file; s <- input$sheet
    req(isTruthy(f), file.exists(f), isTruthy(s))
    f <- normalize_path(f)
    
    clean_chr <- function(x) { x <- as.character(x); x <- trimws(x); x[x == "" | x == "NA"] <- NA_character_; x }
    
    if (supports_variable_view(s)) {
      sel_var <- input$var_picker
      if (nzchar(sel_var)) {
        raw <- tryCatch(openxlsx::read.xlsx(f, sheet = s, colNames = FALSE,
                                            detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE),
                        error = function(e) NULL)
        validate(need(is.data.frame(raw) && nrow(raw) > 0, paste("Cannot read sheet:", s)))
        
        rx <- paste0("^\\s*[•\\-]\\s*", gsub("([\\W])", "\\\\\\1", sel_var), "\\s*\\(")
        title_row <- NA_integer_
        for (r in seq_len(nrow(raw))) {
          row_vals <- as.character(raw[r, , drop = TRUE])
          if (any(grepl(rx, row_vals))) { title_row <- r; break }
        }
        validate(need(!is.na(title_row), paste0("Variable title not found: ", sel_var)))
        
        header_row <- title_row + 1L
        data_row   <- title_row + 2L
        validate(need(header_row <= nrow(raw), "Header row missing under the variable title."))
        validate(need(data_row   <= nrow(raw), "No data line found under the variable title."))
        
        hdr  <- clean_chr(unlist(raw[header_row, , drop = TRUE]))
        vals <- clean_chr(unlist(raw[data_row,   , drop = TRUE]))
        keep <- which(!(is.na(hdr) & is.na(vals)))
        validate(need(length(keep) > 0, "Block has no visible headers/values."))
        
        df <- as.data.frame(t(vals[keep]), stringsAsFactors = FALSE)
        col_names <- hdr[keep]
        fill_idx  <- which(is.na(col_names) | col_names == "")
        if (length(fill_idx)) col_names[fill_idx] <- paste0("Col_", keep[fill_idx])
        col_names <- make.unique(col_names, sep = "_"); names(df) <- col_names
        
        return(DT::datatable(df, options  = list(dom = 't', paging = FALSE, ordering = FALSE, info = FALSE),
                             rownames = FALSE))
      }
    }
    
    df <- tryCatch(openxlsx::read.xlsx(f, sheet = s, colNames = TRUE, detectDates = TRUE,
                                       skipEmptyRows = FALSE, skipEmptyCols = FALSE),
                   error = function(e) NULL)
    validate(need(is.data.frame(df), paste("Cannot read sheet:", s)))
    fix_names <- function(x) { x <- as.character(x); x[is.na(x) | x == ""] <- "X"; make.unique(x, sep = "_") }
    names(df) <- fix_names(names(df))
    if (nrow(df) == 0 && ncol(df) == 0) {
      return(DT::datatable(data.frame(note = sprintf("Sheet '%s' is empty.", s)),
                           options = list(dom = 't', paging = FALSE, ordering = FALSE, info = FALSE),
                           rownames = FALSE))
    }
    DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$img_gallery <- renderUI({
    idx <- rv_img_index()
    if (is.null(idx)) return(div("No images found.", style = "color:#666;"))
    s <- input$sheet; sel_var <- input$var_picker
    
    imgs <- character(0)
    if (supports_variable_view(s)) {
      has_mapping <- !is.null(idx$by_var) && nrow(idx$by_var) > 0
      if (nzchar(sel_var) && has_mapping) {
        subset_rows <- idx$by_var$var == sel_var
        imgs <- unique(idx$by_var$img_path[subset_rows])
        if (!length(imgs)) return(div(sprintf("No images mapped to variable '%s'.", sel_var), style = "color:#666;"))
      } else if (nzchar(sel_var) && !has_mapping) {
        return(tags$div(
          div("Images exist but this sheet has no anchor metadata to associate them to variables.", style = "color:#666; margin-bottom:6px;"),
          div("Tip: plots were likely inserted without anchors for this mission/sheet.", style = "color:#666;")
        ))
      } else {
        imgs <- unique(idx$by_sheet$img_path)
      }
    } else {
      imgs <- unique(idx$by_sheet$img_path)
    }
    
    if (!length(imgs)) return(div("No images available for this sheet/workbook.", style = "color:#666;"))
    
    tags$div(lapply(imgs, function(p) {
      ext <- tolower(file_ext(p))
      mime <- if (ext %in% c("png")) "image/png" else if (ext %in% c("jpg","jpeg")) "image/jpeg" else "image/png"
      uri  <- tryCatch(base64enc::dataURI(file = p, mime = mime), error = function(e) NULL)
      if (is.null(uri)) return(NULL)
      tags$div(
        tags$img(src = uri, style = "max-width:100%; border:1px solid #ddd; margin: 4px 0;"),
        tags$div(basename(p), style = "font-size:12px; color:#666; margin-bottom:10px;")
      )
    }))
  })
  
  # ---------- Data Editor ----------
  # Load from Access DB
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
    
    # Fetch three base tables
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
    
    # Mission filter options
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
  
  # Filtered edits
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
  
  # Save edits to COPY
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
  
  observeEvent(input$save_edits, {
    req(length(rv$changes) > 0)
    ap <- input$accdb_path; cp <- input$accdb_copy
    req(isTruthy(ap), file.exists(ap))
    req(isTruthy(cp))
    
    ok_copy <- try(file.copy(ap, cp, overwrite = TRUE), silent = TRUE)
    if (!isTRUE(ok_copy)) {
      showNotification("Could not create the copy of the ACCDB.", type="error"); return()
    }
    
    con <- try(RODBC::odbcConnectAccess2007(cp, believeNRows = FALSE), silent = TRUE)
    if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
      conn_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", cp, ";Uid=;Pwd=;")
      con <- try(RODBC::odbcDriverConnect(conn_str), silent = TRUE)
      if (inherits(con, "try-error") || is.null(con) || isTRUE(con < 0)) {
        showNotification("ODBC open failed on COPY.", type="error"); return()
      }
    }
    on.exit(RODBC::odbcClose(con), add = TRUE)
    
    n_applied <- 0L
    for (chg in rv$changes) {
      tbl <- chg$table
      set_vals <- setNames(list(chg$new), chg$column)
      sql <- build_update_sql(tbl, key_vals = chg$keys, set_vals = set_vals)
      status <- try(RODBC::sqlQuery(con, sql, errors = TRUE), silent = TRUE)
      if (!inherits(status, "try-error") && is.null(status)) n_applied <- n_applied + 1L
      else append_log("[save][WARN] failed SQL: ", sql)
    }
    showNotification(paste0("Applied ", n_applied, " change(s) to: ", cp), type="message")
    append_log("[save] Applied ", n_applied, " change(s) to: ", cp)
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