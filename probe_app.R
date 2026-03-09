# ---- probe_app.R ----
library(shiny)
library(openxlsx)

ui <- fluidPage(
  titlePanel("SOMEC — Probe (scan + sheets)"),
  sidebarLayout(
    sidebarPanel(
      textInput("parent", "Reports parent folder:",
                value = "C:/Users/BolducF/Documents/ShinyApps/SOMEC/GestionDeDonnees",
                width = "100%"),
      actionButton("refresh", "Refresh list"),
      tags$hr(),
      selectInput("folder", "Reports folder:", choices = character(0)),
      selectInput("file",   "Report file (.xlsx):", choices = character(0))
    ),
    mainPanel(
      h4("Console"), verbatimTextOutput("log", placeholder = TRUE),
      tags$hr(),
      h4("Sheets"), verbatimTextOutput("sheets")
    )
  )
)

server <- function(input, output, session) {
  log <- reactiveVal("")
  append_log <- function(...) {
    line <- paste(format(Sys.time(), "%H:%M:%S"), paste(..., collapse=" "), sep="  ")
    cur <- log()
    log(paste0(cur, if (nzchar(cur)) "\n" else "", line))
  }
  output$log <- renderText(log())
  
  # log immediately so we know server loop is alive
  # session$onFlushed(function() {
  #   append_log("[init] app started")
  # }, once = TRUE)
  
  # scan function (no magic)
  scan_reports <- function(parent) {
    parent <- tryCatch(normalizePath(parent, winslash="/", mustWork=FALSE), error=function(e) parent)
    append_log("[scan] parent=", parent, " exists=", dir.exists(parent))
    
    subs <- if (dir.exists(parent)) list.dirs(parent, full.names=TRUE, recursive=FALSE) else character(0)
    mrs  <- subs[grepl("MissionReports_[0-9]{8}$", basename(subs), perl=TRUE)]
    append_log("[scan] MR folders: ", length(mrs), if (length(mrs)) paste0(" [", paste(basename(mrs), collapse=", "), "]") else "")
    list(parent=parent, mrs=mrs)
  }
  # Safe, one-shot startup log (runs in a reactive context)
  observeEvent(TRUE, {
    append_log("[init] app started")
  }, once = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$reports_parent, {
    append_log("[init] reports_parent = ", input$reports_parent)
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$parent, input$refresh), {
    r <- scan_reports(input$parent)
    if (!length(r$mrs)) {
      updateSelectInput(session, "folder", choices=character(0))
      updateSelectInput(session, "file",   choices=character(0))
      append_log("[scan] no MissionReports_*")
      return()
    }
    # newest first
    pick <- r$mrs[order(basename(r$mrs), decreasing=TRUE)]
    updateSelectInput(session, "folder", choices=setNames(pick, basename(pick)), selected=pick[1])
    
    files <- list.files(pick[1], pattern="\\.xlsx$", full.names=TRUE, ignore.case=TRUE)
    files <- tryCatch(normalizePath(files, winslash="/", mustWork=FALSE), error=function(e) files)
    append_log("[scan] .xlsx in ", basename(pick[1]), ": ", length(files),
               if (length(files)) paste0(" [", paste(basename(head(files,8)), collapse=", "), if (length(files)>8) ", ..." else "", "]") else "")
    updateSelectInput(session, "file", choices=setNames(files, basename(files)), selected=if (length(files)) files[1] else character(0))
  }, ignoreInit = FALSE)
  
  observeEvent(input$file, {
    f <- input$file
    if (!isTruthy(f) || !file.exists(f)) { output$sheets <- renderText("<none>"); return() }
    sh <- tryCatch(openxlsx::getSheetNames(f), error=function(e) NULL)
    output$sheets <- renderText(if (length(sh)) paste(sh, collapse=", ") else "<error/null>")
    append_log("[sheets] for file: ", f, " => ", if (length(sh)) paste(sh, collapse=", ") else "<error/null>")
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)