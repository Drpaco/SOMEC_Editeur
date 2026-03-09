# ============================================================
# SOMEC — QC Viewer (Table + Embedded Images; no toggles)
# - Left pickers show short labels (basenames)
# - One Sheet selector drives both the Table and Images
# - For sheets "Transects"/"Observations":
#     • Variable picker appears (auto-detected "• var (...)")
#     • Table shows ONLY the first line under the selected variable
#     • Images panel shows ONLY images mapped to that variable
# - For sheets "Missions"/"QC_Summary":
#     • Variable picker is hidden
#     • Table shows the entire sheet
#     • Images panel shows all images for that sheet (or workbook fallback)
# - In-app Console logs every step
# ============================================================

library(shiny)
library(DT)
library(openxlsx)
library(xml2)
library(base64enc)
library(tools)

# ---- CONFIG ----
default_reports <- "C:/Users/BolducF/Documents/ShinyApps/SOMEC/GestionDeDonnees"

normalize_path <- function(p) {
  tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
}

supports_variable_view <- function(s) {
  if (!is.character(s) || !nzchar(s)) return(FALSE)
  tolower(s) %in% c("transects","observations")
}

supports_variable_view <- function(s) {
  if (!is.character(s) || !nzchar(s)) return(FALSE)
  tl <- tolower(s)
  tl %in% c("transects","observations")
}

ui <- fluidPage(
  titlePanel("SOMEC — QC Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("reports_parent", "Reports parent folder:", value = default_reports, width = "100%"),
      actionButton("refresh_reports", "Refresh reports"),
      tags$hr(),
      
      # Short labels (basenames) — values are full paths
      selectInput("reports_folder", "Reports folder:", choices = character(0)),
      selectInput("report_file",   "Report file (.xlsx):", choices = character(0)),
      tags$hr(),
      
      # One sheet drives all
      selectInput("sheet", "Sheet:", choices = character(0)),
      # (No toggle for raw cells; we always read with headers)
      tags$hr(),
      
      # Variable view — shown ONLY for Transects/Observations via conditionalPanel
      conditionalPanel(
        condition = "['transects','observations'].includes((input.sheet || '').toLowerCase())",
        h4("Variable view"),
        helpText("Pick a variable title found on the sheet (lines that start with '• ' or '- ')."),
        selectInput("var_picker", "Variable:", choices = character(0))
      ),
      
      tags$hr(),
      actionButton("debug_dump", "Debug dump")
    ),
    
    mainPanel(
      h4("Status"),
      textOutput("qc_status"),
      tags$hr(),
      
      h4("QC table"),
      div(textOutput("var_label"), style = "margin: -6px 0 8px 0; color:#555;"),
      DTOutput("qc_table"),
      tags$hr(),
      
      h4("Embedded images"),
      uiOutput("img_gallery"),
      tags$hr(),
      
      h4("Console (from the app)"),
      verbatimTextOutput("app_console", placeholder = TRUE)
    )
  )
)

server <- function(input, output, session) {
  # ---------- In-app console ----------
  log_buf <- ""
  append_log <- function(...) {
    ts <- format(Sys.time(), "%H:%M:%S")
    line <- paste(ts, paste(..., collapse = " "), sep = "  ")
    log_buf <<- if (nzchar(log_buf)) paste(log_buf, line, sep = "\n") else line
  }
  output$app_console <- renderText({ invalidateLater(500, session); log_buf })
  outputOptions(output, "app_console", suspendWhenHidden = FALSE)
  
  session$onFlushed(function() append_log("[init] app started"), once = TRUE)
  
  # ---------- Scan helpers ----------
  folder_rx <- "^MissionReports_[0-9]{8}$"
  
  # ---------- Refresh: list MissionReports_* ----------
  observeEvent(input$refresh_reports, {
    parent <- normalize_path(input$reports_parent)
    append_log("[refresh] parent =", parent, " exists=", dir.exists(parent))
    
    if (!dir.exists(parent)) {
      updateSelectInput(session, "reports_folder", choices = character(0))
      updateSelectInput(session, "report_file",   choices = character(0))
      updateSelectInput(session, "sheet",         choices = character(0))
      updateSelectInput(session, "var_picker",    choices = character(0))
      return()
    }
    
    subs <- list.dirs(parent, full.names = TRUE, recursive = FALSE)
    keep <- subs[grepl(folder_rx, basename(subs), ignore.case = TRUE)]
    keep <- keep[order(basename(keep), decreasing = TRUE)]
    append_log("[scan] MR folders:", if (length(keep)) paste(basename(keep), collapse = ", ") else "<none>")
    
    if (length(keep)) {
      labs <- setNames(keep, basename(keep))               # short labels
      updateSelectInput(session, "reports_folder", choices = labs, selected = keep[1])
      append_log("[ui] reports_folder selected:", keep[1])
    } else {
      updateSelectInput(session, "reports_folder", choices = character(0))
      updateSelectInput(session, "report_file",   choices = character(0))
      updateSelectInput(session, "sheet",         choices = character(0))
      updateSelectInput(session, "var_picker",    choices = character(0))
      append_log("[ui] no MR folders found")
    }
  }, ignoreInit = FALSE)
  
  # ---------- Folder -> list .xlsx (short labels) ----------
  observeEvent(input$reports_folder, {
    folder <- input$reports_folder
    if (!isTruthy(folder) || !dir.exists(folder)) {
      updateSelectInput(session, "report_file", choices = character(0))
      updateSelectInput(session, "sheet",       choices = character(0))
      updateSelectInput(session, "var_picker",  choices = character(0))
      append_log("[ui] invalid reports_folder")
      return()
    }
    files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
    files <- normalize_path(files)
    append_log("[scan] files:", if (length(files)) paste(basename(files), collapse = ", ") else "<none>")
    
    if (length(files)) {
      idx <- grep("SOMEC_Mission_QAQC_Index_", basename(files), ignore.case = TRUE)
      if (length(idx)) files <- c(files[idx], files[-idx])
      labs <- setNames(files, basename(files))             # short labels
      updateSelectInput(session, "report_file", choices = labs, selected = files[1])
      append_log("[ui] report_file selected:", files[1])
    } else {
      updateSelectInput(session, "report_file", choices = character(0))
      updateSelectInput(session, "sheet",       choices = character(0))
      updateSelectInput(session, "var_picker",  choices = character(0))
      append_log("[ui] no .xlsx in selected folder")
    }
  }, ignoreInit = TRUE)
  
  # ---------- File -> list sheets ----------
  observeEvent(input$report_file, {
    f <- input$report_file
    if (!isTruthy(f) || !file.exists(f)) {
      updateSelectInput(session, "sheet",      choices = character(0))
      updateSelectInput(session, "var_picker", choices = character(0))
      append_log("[ui] invalid report_file")
      return()
    }
    f <- normalize_path(f)
    append_log("[workbook] open:", f)
    
    wb <- tryCatch(openxlsx::loadWorkbook(f), error = function(e) NULL)
    if (is.null(wb)) {
      updateSelectInput(session, "sheet",      choices = character(0))
      updateSelectInput(session, "var_picker", choices = character(0))
      append_log("[workbook][ERROR] cannot open")
      return()
    }
    
    sheets <- names(wb)
    append_log("[workbook] sheets:", paste(sheets, collapse = ", "))
    if (!length(sheets)) {
      updateSelectInput(session, "sheet",      choices = character(0))
      updateSelectInput(session, "var_picker", choices = character(0))
      return()
    }
    
    low <- tolower(sheets)
    pick_first <- function(cand) { hit <- which(low == cand); if (length(hit)) sheets[hit[1]] else NA_character_ }
    pref <- c(pick_first("transects"), pick_first("observations"),
              pick_first("qc_summary"), pick_first("missions"),
              pick_first("index"), sheets[1])
    pref <- pref[!is.na(pref)][1]
    
    updateSelectInput(session, "sheet", choices = sheets, selected = pref)
    append_log("[ui] sheet selected:", pref)
  }, ignoreInit = TRUE)
  
  # ---------- Status ----------
  output$qc_status <- renderText({
    f <- input$report_file
    if (!isTruthy(f)) return("Selected: <none>")
    f <- normalize_path(f)
    paste0("Selected: ", f, if (file.exists(f)) " (exists)" else " (not found)")
  })
  outputOptions(output, "qc_status", suspendWhenHidden = FALSE)
  
  # ---------- Variable titles detector ----------
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
        grepl("^\\s*[•\\-]\\s*.+\\(", cell)   # "• name (" or "- name ("
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
  
  # ---------- Images: anchors + mapping ----------
  sheet_index_of <- function(file, sheet_name) {
    sn <- tryCatch(openxlsx::getSheetNames(file), error = function(e) NULL)
    if (is.null(sn)) return(NA_integer_)
    hit <- which(sn == sheet_name)
    if (length(hit)) hit[1] else NA_integer_
  }
  resolve_target <- function(tmp_root, base_dir, target) {
    if (grepl("^/", target)) {
      return(normalizePath(file.path(tmp_root, sub("^/", "", target)),
                           winslash = "/", mustWork = FALSE))
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
    
    rels <- tryCatch(xml2::read_xml(sheet_rels), error = function(e) NULL)
    if (is.null(rels)) return(out)
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
      drawing_xml <- resolve_target(tmpdir, file.path(tmpdir, "xl", "worksheets", "_rels"), t)
      if (!grepl("xl/drawings/", drawing_xml, fixed = TRUE)) {
        maybe <- normalizePath(file.path(tmpdir, "xl", "drawings", basename(drawing_xml)), winslash = "/", mustWork = FALSE)
        if (file.exists(maybe)) drawing_xml <- maybe
      }
      drawing_rels_xml <- sub("xl/drawings/drawing([0-9]+)\\.xml$",
                              "xl/drawings/_rels/drawing\\1.xml.rels", drawing_xml)
      if (!file.exists(drawing_rels_xml)) next
      
      d_rels <- tryCatch(xml2::read_xml(drawing_rels_xml), error = function(e) NULL)
      if (is.null(d_rels)) next
      d_nodes <- xml2::xml_find_all(d_rels, ".//*[local-name()='Relationship']")
      d_rId   <- xml2::xml_attr(d_nodes, "Id")
      d_tgt   <- xml2::xml_attr(d_nodes, "Target")
      id_to_img <- setNames(
        vapply(d_tgt, function(it) {
          imgp <- resolve_target(tmpdir, file.path(tmpdir, "xl", "drawings", "_rels"), it)
          if (!grepl("xl/media/", imgp, fixed = TRUE)) {
            maybe <- normalizePath(file.path(tmpdir, "xl", "media", basename(imgp)),
                                   winslash = "/", mustWork = FALSE)
            if (file.exists(maybe)) imgp <- maybe
          }
          imgp
        }, character(1)),
        d_rId
      )
      
      dxml <- tryCatch(xml2::read_xml(drawing_xml), error = function(e) NULL)
      if (is.null(dxml)) next
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
  
  extract_all_images <- function(xlsx) {
    tmpdir <- tempfile("unz_"); dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
    utils::unzip(xlsx, exdir = tmpdir)
    media_dir <- file.path(tmpdir, "xl", "media")
    if (!dir.exists(media_dir)) return(character(0))
    list.files(media_dir, full.names = TRUE)
  }
  
  # Store mapping results
  rv_img_index <- reactiveVal(NULL) # list: by_sheet(df img,row,col), by_var(df var,img,row,col), vars(df var,row_start)
  
  # Build mapping whenever file OR sheet changes
  observeEvent(list(input$report_file, input$sheet), {
    f <- input$report_file; s <- input$sheet
    if (!isTruthy(f) || !file.exists(f) || !isTruthy(s)) {
      rv_img_index(NULL); updateSelectInput(session, "var_picker", choices = character(0)); return()
    }
    f <- normalize_path(f)
    idx <- sheet_index_of(f, s)
    append_log("[images] extract anchors for sheet:", s, "(index=", idx, ")")
    
    # Sheet-linked images
    by_sheet <- tryCatch(extract_images_with_anchors(f, idx), error = function(e) NULL)
    # Titles (only if sheet supports variable view)
    vars <- if (supports_variable_view(s)) detect_variable_titles(f, s) else data.frame(var=character(0), row_start=integer(0))
    if (nrow(vars)) append_log("[vars] detected:", paste(vars$var, collapse=" | "))
    
    # If no sheet-linked images, fallback to all workbook images
    if (is.null(by_sheet) || !nrow(by_sheet)) {
      append_log("[images] no sheet-linked images; fallback to all /xl/media")
      imgs_all <- extract_all_images(f)
      rv_img_index(list(
        by_sheet = if (length(imgs_all)) data.frame(img_path = imgs_all, row = NA_integer_, col = NA_integer_) else data.frame(img_path=character(0),row=integer(0),col=integer(0)),
        by_var   = data.frame(var=character(0), img_path=character(0), row=integer(0), col=integer(0)),
        vars     = vars
      ))
    } else {
      append_log("[images] sheet-linked images:", paste(basename(by_sheet$img_path), collapse=", "))
      # Map image to nearest variable title above it (only for variable sheets)
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
    
    # Populate/clear variable picker depending on sheet
    if (supports_variable_view(s) && nrow(vars)) {
      updateSelectInput(session, "var_picker", choices = vars$var, selected = vars$var[1])
    } else {
      updateSelectInput(session, "var_picker", choices = character(0))
    }
  }, ignoreInit = TRUE)
  
  # ---------- TABLE ----------
  # - If sheet supports variable view AND a variable is selected -> show only the first data row under that title
  # - Else -> show entire sheet
  
  # Shows "Variable: <name>" under the QC table (only for Transects/Observations)
  output$var_label <- renderText({
    # helper must exist; if you don't have it, see step 3 below
    if (!supports_variable_view(input$sheet)) return("")  # hide on Missions / QC_Summary
    v <- input$var_picker
  })
  
  # --- Keep var_picker strictly in sync with the selected Sheet ---
  observeEvent(input$sheet, {
    f <- input$report_file
    s <- input$sheet
    
    # No file or no sheet -> clear
    if (!isTruthy(f) || !file.exists(f) || !isTruthy(s)) {
      updateSelectInput(session, "var_picker", choices = character(0))
      return()
    }
    
    # For Missions / QC_Summary: no variables
    if (!supports_variable_view(s)) {
      updateSelectInput(session, "var_picker", choices = character(0))
      return()
    }
    
    # Detect variables only for Transects / Observations
    f <- normalize_path(f)
    vars <- detect_variable_titles(f, s)  # returns data.frame(var, row_start)
    
    if (nrow(vars)) {
      updateSelectInput(session, "var_picker", choices = vars$var, selected = vars$var[1])
    } else {
      updateSelectInput(session, "var_picker", choices = character(0))
    }
  }, ignoreInit = FALSE)
    
  output$qc_table <- DT::renderDT({
    f <- input$report_file
    s <- input$sheet
    req(isTruthy(f), file.exists(f), isTruthy(s))
    f <- normalize_path(f)
    
    clean_chr <- function(x) {
      x <- as.character(x)
      x <- trimws(x)
      x[x == "" | x == "NA"] <- NA_character_
      x
    }
    
    if (supports_variable_view(s)) {
      sel_var <- input$var_picker
      if (nzchar(sel_var)) {
        raw <- tryCatch(
          openxlsx::read.xlsx(
            f, sheet = s, colNames = FALSE,
            detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE
          ),
          error = function(e) NULL
        )
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
        if (length(fill_idx)) {
          col_names[fill_idx] <- paste0("Col_", keep[fill_idx])
        }
        col_names <- make.unique(col_names, sep = "_")
        names(df) <- col_names
        
        return(DT::datatable(
          df,
          options  = list(dom = 't', paging = FALSE, ordering = FALSE, info = FALSE),
          rownames = FALSE
        ))
      }
    }
    
    # Full-sheet mode (Missions, QC_Summary, or no variable selected)
    df <- tryCatch(
      openxlsx::read.xlsx(
        f, sheet = s, colNames = TRUE, detectDates = TRUE,
        skipEmptyRows = FALSE, skipEmptyCols = FALSE
      ),
      error = function(e) NULL
    )
    validate(need(is.data.frame(df), paste("Cannot read sheet:", s)))
    
    fix_names <- function(x) { x <- as.character(x); x[is.na(x) | x == ""] <- "X"; make.unique(x, sep = "_") }
    names(df) <- fix_names(names(df))
    
    if (nrow(df) == 0 && ncol(df) == 0) {
      return(DT::datatable(
        data.frame(note = sprintf("Sheet '%s' is empty.", s)),
        options = list(dom = 't', paging = FALSE, ordering = FALSE, info = FALSE),
        rownames = FALSE
      ))
    }
    
    DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  outputOptions(output, "qc_table", suspendWhenHidden = FALSE)
  
  # ---------- IMAGES ----------
  output$img_gallery <- renderUI({
    idx <- rv_img_index()
    if (is.null(idx)) return(div("No images found.", style = "color:#666;"))
    
    s <- input$sheet
    sel_var <- input$var_picker
    
    imgs <- character(0)
    if (supports_variable_view(s)) {
      # Variable-driven filter
      has_mapping <- !is.null(idx$by_var) && nrow(idx$by_var) > 0
      if (nzchar(sel_var) && has_mapping) {
        subset_rows <- idx$by_var$var == sel_var
        imgs <- unique(idx$by_var$img_path[subset_rows])
        if (!length(imgs)) {
          return(div(sprintf("No images mapped to variable '%s'.", sel_var), style = "color:#666;"))
        }
      } else if (nzchar(sel_var) && !has_mapping) {
        return(tags$div(
          div("Images exist but this sheet has no anchor metadata to associate them to variables.", style = "color:#666; margin-bottom:6px;"),
          div("Tip: plots were likely inserted without anchors for this mission/sheet.", style = "color:#666;")
        ))
      } else {
        imgs <- unique(idx$by_sheet$img_path)
      }
    } else {
      # Missions / QC_Summary (no variable filtering)
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
  outputOptions(output, "img_gallery", suspendWhenHidden = FALSE)
  
  # ---------- Debug dump ----------
  observeEvent(input$debug_dump, {
    parent <- normalize_path(input$reports_parent)
    append_log("[dump] parent:", parent)
    if (!dir.exists(parent)) { append_log("[dump] parent DOES NOT EXIST"); return() }
    subs <- list.dirs(parent, full.names = TRUE, recursive = FALSE)
    append_log("[dump] subdirs:\n  - ", if (length(subs)) paste(subs, collapse="\n  - ") else "<none>")
    mrs <- subs[grepl(folder_rx, basename(subs), ignore.case = TRUE)]
    if (length(mrs)) {
      for (d in mrs) {
        fs <- list.files(d, pattern="\\.xlsx$", full.names=TRUE, ignore.case=TRUE)
        append_log("[dump] files in ", d, ":\n  - ", if (length(fs)) paste(fs, collapse="\n  - ") else "<none>")
      }
    } else {
      append_log("[dump] MR folders matched: <none>")
    }
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
