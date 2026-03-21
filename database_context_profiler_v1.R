# ============================================================
# FILE: database_context_profiler_v1.R
# PURPOSE:
#   Build GLOBAL database-wide QC baselines
#   Independent from mission profiler
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(openxlsx)
  library(RODBC)
})

cfg <- list(
  accdb_path = "C:/Users/BolducF/Documents/ShinyApps/SOMEC/BaseDeDonnees/SOMEC_20251106.accdb",
  relcatalog_xlsx = "C:/Users/BolducF/Documents/ShinyApps/SOMEC/GestionDeDonnees/RelCatalog.xlsx",
  relcatalog_sheet = "RelCatalog",
  
  out_dir = file.path(
    "C:/Users/BolducF/Documents/ShinyApps/SOMEC/GestionDeDonnees",
    "GlobalContext"
  ),
  
  force_rebuild = FALSE,   # <<< ADD THIS LINE
  
  suspect_pct_global = 1,
  suspect_n_global = 100
)

cache_dir <- file.path(cfg$out_dir, "_cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
context_rds  <- file.path(cfg$out_dir, "global_context.rds")
context_xlsx <- file.path(cfg$out_dir, "Global_Database_Context.xlsx")


load_access_table_cached <- function(
    accdb_path,
    table_name,
    cache_dir,
    force_reload = FALSE
) {
  
  cache_file <- file.path(cache_dir, paste0(table_name, ".rds"))
  
  # ---- Cache validity check --------------------------------
  cache_valid <- file.exists(cache_file) &&
    file.info(cache_file)$mtime >= file.info(accdb_path)$mtime
  
  if (!force_reload && cache_valid) {
    message("✔ Using cached table: ", table_name)
    return(readRDS(cache_file))
  }
  
  # ---- Load from Access ------------------------------------
  message("⟳ Loading table from Access: ", table_name)
  
  con <- RODBC::odbcConnectAccess2007(accdb_path, believeNRows = FALSE)
  on.exit(RODBC::odbcClose(con), add = TRUE)
  
  df <- RODBC::sqlFetch(con, table_name)
  
  # ---- Save to cache ---------------------------------------
  saveRDS(df, cache_file)
  
  message("💾 Cached table: ", table_name)
  
  df
}

# ---- Excel styles (define once) ----
style_ok  <- createStyle(fontColour = "#1B7837")  # green
style_bad <- createStyle(fontColour = "#B2182B")  # red

# ---- Look-alike helper ----
library(stringdist)

suggest_lookalikes <- function(unknown, allowed, max_dist = 2, n = 3) {
  d <- stringdist(unknown, allowed, method = "lv")
  tibble(candidate = allowed, distance = d) |>
    filter(distance <= max_dist) |>
    arrange(distance) |>
    slice_head(n = n)
}

# ============================================================
# SKIP / REBUILD GUARD
# ============================================================

if (isFALSE(cfg$force_rebuild) &&
    file.exists(context_rds) &&
    file.exists(context_xlsx)) {
  
  message("Global context already exists — skipping rebuild.")
  global_context <- readRDS(context_rds)
  
} else {
# =========================================
# A) LOAD DATA + BUILD CONTEXT
# =========================================

missions <- load_access_table_cached(
  cfg$accdb_path, "missions", cache_dir
) %>% clean_names()

transects <- load_access_table_cached(
  cfg$accdb_path, "transects", cache_dir
) %>% clean_names()

observations <- load_access_table_cached(
  cfg$accdb_path, "observations", cache_dir
) %>% clean_names()


relcatalog_df <- read.xlsx(
  cfg$relcatalog_xlsx,
  sheet = cfg$relcatalog_sheet
) %>% clean_names()

message("RelCatalog columns:")
print(names(relcatalog_df))

# message("Sample of refobject / refcolumn:")
# print(
#   relcatalog_df %>%
#     select(object, refobject, refcolumn) %>%
#     slice_head(n = 10)
# )

# ============================================================
#  — Catalog bootstrap (INLINED from catalog_loader.R)
# ============================================================

# ---- Helpers (copied verbatim where relevant) --------------

normalize_name <- function(s) {
  s <- as.character(s)
  s <- gsub("\\s+", " ", s)
  trimws(s)
}

resolve_access_table <- function(candidate, all_names) {
  cand <- normalize_name(candidate)
  if (candidate %in% all_names) return(candidate)
  if (paste0("[", candidate, "]") %in% all_names) return(paste0("[", candidate, "]"))
  an <- normalize_name(all_names)
  hit <- which(tolower(an) == tolower(cand))
  if (length(hit)) return(all_names[hit[1]])
  hit2 <- which(grepl(paste0("\\b", gsub("\\s+","\\s+", tolower(cand)), "\\b"), tolower(an)))
  if (length(hit2)) return(all_names[hit2[1]])
  NA_character_
}

resolve_col_name <- function(candidate, cols,
                             key_candidates = c("code","id","value","valeur","cle","clef")) {
  if (candidate %in% cols) return(candidate)
  n_cols <- tolower(normalize_name(cols))
  n_cand <- tolower(normalize_name(candidate))
  hit <- which(n_cols == n_cand)
  if (length(hit)) return(cols[hit[1]])
  n_cands <- tolower(normalize_name(c(candidate, key_candidates)))
  hit2 <- which(n_cols %in% n_cands)
  if (length(hit2)) return(cols[hit2[1]])
  NA_character_
}

as_cat_objname <- function(table_name) {
  paste0("cat_", make.names(gsub("\\s+","_", table_name)))
}

# ---- Normalize RelCatalog exactly like loader ---------------

rn <- tolower(names(relcatalog_df))
relcatalog_df <- tibble(
  Object     = normalize_name(relcatalog_df[[which.max(rn %in% c("object"))]]),
  ColumnName = normalize_name(relcatalog_df[[which.max(rn %in% c("column_name","column"))]]),
  RefObject  = normalize_name(relcatalog_df[[which.max(rn %in% c("ref_object"))]]),
  RefColumn  = normalize_name(relcatalog_df[[which.max(rn %in% c("ref_column"))]])
) %>%
  filter(!is.na(Object), !is.na(RefObject), !is.na(RefColumn))

# ---- Connect to Access (single, controlled connection) ------

con <- odbcConnectAccess2007(cfg$accdb_path, believeNRows = FALSE)
if (is.null(con)) stop("Could not open Access DB")

acc_tables <- sqlTables(con)$TABLE_NAME

# ---- Identify catalog rows (same rule as loader) ------------

data_tables <- c("missions","transects","observations")

rows <- relcatalog_df %>%
  mutate(
    obj_is_data = tolower(Object) %in% data_tables,
    ref_is_data = tolower(RefObject) %in% data_tables,
    is_catalog_row = xor(obj_is_data, ref_is_data),
    catalog_table = ifelse(ref_is_data & !obj_is_data, Object,
                           ifelse(obj_is_data & !ref_is_data, RefObject, NA_character_)),
    catalog_column = ifelse(ref_is_data & !obj_is_data, ColumnName,
                            ifelse(obj_is_data & !ref_is_data, RefColumn, NA_character_)),
    data_table  = ifelse(ref_is_data & !obj_is_data, RefObject,
                         ifelse(obj_is_data & !ref_is_data, Object, NA_character_)),
    data_column = ifelse(ref_is_data & !obj_is_data, RefColumn,
                         ifelse(obj_is_data & !ref_is_data, ColumnName, NA_character_))
  ) %>%
  filter(is_catalog_row)

# ---- Load catalog tables (cat_*) ----------------------------

catalog_map_df <- rows %>%
  distinct(catalog_table) %>%
  mutate(
    access_table = vapply(catalog_table, resolve_access_table,
                          character(1), all_names = acc_tables),
    found = !is.na(access_table)
  )

for (i in seq_len(nrow(catalog_map_df))) {
  if (!catalog_map_df$found[i]) next
  df <- tryCatch(sqlFetch(con, catalog_map_df$access_table[i]), error = function(e) NULL)
  if (is.null(df)) next
  assign(
    as_cat_objname(catalog_map_df$catalog_table[i]),
    df,
    envir = .GlobalEnv
  )
}

# ---- Resolve catalog columns --------------------------------

catalog_map_df <- rows %>%
  mutate(
    cat_obj = vapply(catalog_table, function(ct) {
      obj <- as_cat_objname(ct)
      if (exists(obj, envir = .GlobalEnv)) obj else NA_character_
    }, character(1)),
    catalog_column_resolved = mapply(
      function(obj, col) {
        if (is.na(obj)) return(NA_character_)
        resolve_col_name(col, names(get(obj, envir = .GlobalEnv)))
      },
      cat_obj, catalog_column
    ),
    ok = !is.na(cat_obj) & !is.na(catalog_column_resolved)
  ) %>%
  transmute(
    data_table, data_column,
    cat_obj,
    catalog_column = catalog_column_resolved,
    ok
  )

# ---- Build EXACT same catalog map ----------------------------

build_catalog_map <- function() {
  mp <- list()
  for (i in seq_len(nrow(catalog_map_df))) {
    if (!isTRUE(catalog_map_df$ok[i])) next
    key <- paste0(
      tolower(catalog_map_df$data_table[i]),
      "$",
      tolower(catalog_map_df$data_column[i])
    )
    obj <- catalog_map_df$cat_obj[i]
    col <- catalog_map_df$catalog_column[i]
    vals <- unique(na.omit(as.character(get(obj, envir = .GlobalEnv)[[col]])))
    mp[[key]] <- vals
  }
  mp
}

catalog_map <- build_catalog_map()

odbcClose(con)

message("Catalog map built: ", length(catalog_map), " mapped fields")
print(head(names(catalog_map)))



head(names(catalog_map))
length(catalog_map)

#step 6

global_cat_freqs <- function(df, table_name, cat_map) {
  
  map_dfr(names(df), function(col) {
    
    key <- paste0(table_name, "$", col)
    if (is.null(cat_map[[key]])) return(NULL)
    
    df %>%
      transmute(value = as.character(.data[[col]])) %>%
      filter(!is.na(value)) %>%
      count(value, name = "n_global") %>%
      mutate(
        table = table_name,
        column = col,
        pct_global = 100 * n_global / sum(n_global)
      )
  })
}

cat_global <- bind_rows(
  global_cat_freqs(transects,    "transects",    catalog_map),
  global_cat_freqs(observations, "observations", catalog_map)
)

# ---- Build categorical presence across missions (FINAL & SAFE) ----

n_missions_total <- n_distinct(missions$mission)

compute_cat_presence <- function(df, table_name, catalog_map) {
  
  out <- list()
  
  for (col in names(df)) {
    
    key <- paste0(tolower(table_name), "$", tolower(col))
    if (is.null(catalog_map[[key]])) next   # only catalog-backed columns
    
    tmp <- df %>%
      select(mission, !!sym(col)) %>%
      filter(!is.na(.data[[col]])) %>%
      distinct(mission, value = as.character(.data[[col]])) %>%
      count(value, name = "n_missions") %>%
      mutate(
        table = table_name,
        name  = col,
        pct_missions = 100 * n_missions / n_missions_total
      )
    
    out[[length(out) + 1]] <- tmp
  }
  
  bind_rows(out)
}

cat_presence <- bind_rows(
  compute_cat_presence(transects,    "transects",    catalog_map),
  compute_cat_presence(observations, "observations", catalog_map)
)





catalog_suspects <- cat_global %>%
  rowwise() %>%
  filter(
    !(value %in% catalog_map[[paste0(table, "$", column)]]) &
      (pct_global >= cfg$suspect_pct_global | n_global >= cfg$suspect_n_global)
  ) %>%
  ungroup() %>%
  arrange(desc(n_global))

global_numeric <- function(df, table_name) {
  
  num_cols <- df %>% select(where(is.numeric)) %>% names()
  
  map_dfr(num_cols, function(col) {
    x <- df[[col]]
    tibble(
      table = table_name,
      column = col,
      n = sum(!is.na(x)),
      p05 = quantile(x, 0.05, na.rm = TRUE),
      p50 = quantile(x, 0.50, na.rm = TRUE),
      p95 = quantile(x, 0.95, na.rm = TRUE)
    )
  })
}

numeric_global <- bind_rows(
  global_numeric(transects,    "transects"),
  global_numeric(observations, "observations")
)



# ---- Column health diagnostics (SAFE, no pivot) ----

compute_column_health <- function(df, table_name) {
  
  out <- list()
  
  for (col in names(df)) {
    
    tmp <- df %>%
      transmute(
        mission = mission,
        value   = .data[[col]]
      )
    
    out[[length(out) + 1]] <- tibble(
      table = table_name,
      name  = col,
      pct_na = mean(is.na(tmp$value)) * 100,
      n_missions_present = n_distinct(tmp$mission[!is.na(tmp$value)]),
      n_types = n_distinct(vapply(tmp$value, function(x) class(x)[1], character(1)))    )
  }
  
  bind_rows(out)
}

column_health <- bind_rows(
  compute_column_health(transects,    "transects"),
  compute_column_health(observations, "observations")
)



relationship_qc <- tibble(
  missions_without_transects =
    sum(!missions$mission %in% transects$mission),
  
  missions_without_observations =
    sum(!missions$mission %in% observations$mission),
  
  observations_without_transects =
    sum(!observations$transect_id %in% transects$transect_id)
)

global_context <- list(
  categorical_freqs   = cat_global,
  categorical_presence = cat_presence,
  catalog_suspects    = catalog_suspects,
  numeric_baselines   = numeric_global,
  column_health       = column_health,
  relationships       = relationship_qc,
  catalog_map         = catalog_map
)

saveRDS(
  global_context,
  file.path(cfg$out_dir, "global_context.rds")
)


# =========================================
# B) EXCEL OUTPUT (FINAL, CONSISTENT)
# =========================================
n_missions_total <- n_distinct(missions$mission)

wb <- createWorkbook()

# -------------------------------------------------
# 1) CATEGORICAL VARIABLES (with look-alikes)
# -------------------------------------------------

cat_global <- cat_global |>
  rowwise() |>
  mutate(
    in_catalog = {
      key <- paste0(tolower(table), "$", tolower(column))
      allowed <- catalog_map[[key]]
      if (is.null(allowed)) NA
      else value %in% allowed
    },
    suggested_codes = if (!isTRUE(in_catalog)) {
      key <- paste0(tolower(table), "$", tolower(column))
      allowed <- catalog_map[[key]]
      
      if (is.null(allowed)) {
        NA_character_
      } else {
        s <- suggest_lookalikes(value, allowed)
        if (nrow(s))
          paste0(s$candidate, " (d=", s$distance, ")", collapse = ", ")
        else
          NA_character_
      }
    } else {
      NA_character_
    }
  ) |>
  ungroup()

addWorksheet(wb, "Categorical_Global")
writeData(wb, "Categorical_Global", cat_global)

# Row-level formatting
all_cols  <- 1:ncol(cat_global)
data_rows <- 2:(nrow(cat_global) + 1)

in_catalog_col    <- which(names(cat_global) == "in_catalog")
in_catalog_letter <- int2col(in_catalog_col)

# GREEN = valid
conditionalFormatting(
  wb, "Categorical_Global",
  cols = all_cols,
  rows = data_rows,
  rule = paste0("$", in_catalog_letter, "2=TRUE"),
  style = createStyle(fontColour = "#1B7837")
)

# RED = invalid
conditionalFormatting(
  wb, "Categorical_Global",
  cols = all_cols,
  rows = data_rows,
  rule = paste0("$", in_catalog_letter, "2=FALSE"),
  style = createStyle(fontColour = "#B2182B")
)

# # -------------------------------------------------
# # 2) CATALOG SUSPECTS
# # -------------------------------------------------
# 
# addWorksheet(wb, "Catalog_Suspects")
# writeData(wb, "Catalog_Suspects", catalog_suspects)

# -------------------------------------------------
# 3) NUMERIC VARIABLES (global summaries)
# -------------------------------------------------

addWorksheet(wb, "Numeric_Global")
writeData(wb, "Numeric_Global", numeric_global)

# (Optional: later you can add qc_flag-based coloring here)

# -------------------------------------------------
# 4) COLUMN HEALTH
# -------------------------------------------------
column_health <- column_health |>
  mutate(
    qc_flag = ifelse(
      n_missions_present < n_missions_total,
      "PARTIAL_PRESENCE",
      "OK"
    )
  )

# -------------------------------------------------
# 4) COLUMN HEALTH
# -------------------------------------------------

addWorksheet(wb, "Column_Health")
writeData(wb, "Column_Health", column_health)

all_cols  <- 1:ncol(column_health)
data_rows <- 2:(nrow(column_health) + 1)

qc_col        <- which(names(column_health) == "qc_flag")
qc_col_letter <- int2col(qc_col)

# RED rows = partial presence across missions
conditionalFormatting(
  wb, "Column_Health",
  cols = all_cols,
  rows = data_rows,
  rule = paste0("$", qc_col_letter, "2<>\"OK\""),
  style = createStyle(fontColour = "#B2182B")
)

# GREEN rows = OK
conditionalFormatting(
  wb, "Column_Health",
  cols = all_cols,
  rows = data_rows,
  rule = paste0("$", qc_col_letter, "2=\"OK\""),
  style = createStyle(fontColour = "#1B7837")
)

# -------------------------------------------------
# 5) RELATIONSHIPS
# -------------------------------------------------

addWorksheet(wb, "Relationships")
writeData(wb, "Relationships", relationship_qc)

# -------------------------------------------------
# SAVE
# -------------------------------------------------

saveWorkbook(
  wb,
  file.path(cfg$out_dir, "Global_Database_Context.xlsx"),
  overwrite = TRUE
)

global_context$catalog_map <- catalog_map

}


