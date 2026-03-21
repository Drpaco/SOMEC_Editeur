# ============================================================
# FILE: catalog_bootstrap_somerc.R
# PURPOSE:
#   Prepare catalog objects exactly as required by
#   mission_profiler_somerc_v6_3_4.R
#   WITHOUT running any mission profiling
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
  library(RODBC)
  library(janitor)
})

cfg <- list(
  relcatalog_xlsx  = "U:/SOMEC/BaseDeDonnees/GestionDeDonnees/RelCatalog_YYYYMMDD.xlsx",
  relcatalog_sheet = "RelCatalog",
  accdb_path       = "U:/SOMEC/BaseDeDonnees/SOMEC_20251106.accdb"
)

# ---- Load RelCatalog --------------------------------------

relcatalog_df <- read.xlsx(
  cfg$relcatalog_xlsx,
  sheet = cfg$relcatalog_sheet
) %>% clean_names()

# ---- Load reference tables mentioned in RelCatalog --------

ref_tables <- unique(relcatalog_df$object)

con <- odbcConnectAccess2007(cfg$accdb_path, believeNRows = FALSE)
on.exit(odbcClose(con), add = TRUE)

for (tbl in ref_tables) {
  df <- tryCatch(sqlFetch(con, tbl), error = function(e) NULL)
  if (!is.null(df)) {
    assign(
      paste0("cat_", make.names(tolower(tbl))),
      df %>% clean_names(),
      envir = .GlobalEnv
    )
  }
}

# ---- Build catalog map EXACTLY like the profiler -----------

build_catalog_map <- function() {
  mp <- list()
  
  for (i in seq_len(nrow(relcatalog_df))) {
    row <- relcatalog_df[i, ]
    
    key <- paste0(
      tolower(row$refobject),
      "$",
      tolower(row$refcolumn)
    )
    
    cat_name <- paste0("cat_", make.names(tolower(row$object)))
    
    if (!exists(cat_name, envir = .GlobalEnv)) next
    
    cat_df <- get(cat_name, envir = .GlobalEnv)
    
    allowed <- unique(as.character(cat_df[[1]]))  # first column = code
    
    mp[[key]] <- allowed
  }
  
  mp
}

catalog_map_df <- build_catalog_map()
``