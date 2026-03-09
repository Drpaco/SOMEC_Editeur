# ============================================================
# FILE A: catalog_loader.R
# Load RelCatalog from XLSX + load referenced catalog tables from Access
# Produces:
#   - relcatalog_df, relcatalog_diag_df (tibbles)
#   - cat_* data frames (one per catalog table found in Access)
#   - catalog_map_df: tidy mapping (data_table, data_column, catalog_table, catalog_column)
#   - build_catalog_map(): returns a named list mapping "table$col" -> allowed values (character vector)
# ============================================================

suppressPackageStartupMessages({
  library(RODBC)
  library(openxlsx)
  library(tidyverse)
})

# ---------------- CONFIG (EDIT THESE TWO LINES ONLY) ----------------
relcatalog_xlsx  <- "U:/SOMEC/BaseDeDonnees/GestionDeDonnees/RelCatalog.xlsx"  # <-- update date
relcatalog_sheet <- "RelCatalog"

accdb_path <- "U:/SOMEC/BaseDeDonnees/SOMEC_20251106.accdb"
# -------------------------------------------------------------------

message("=== Catalog Loader start ===")
message("RelCatalog XLSX: ", relcatalog_xlsx)
message("Access DB:       ", accdb_path)

# ---------- Helpers ----------
normalize_name <- function(s) {
  s <- as.character(s)
  s <- gsub("\\s+"," ", s)
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
resolve_col_name <- function(candidate, cols, key_candidates = c("code","id","value","valeur","cle","clef")) {
  if (candidate %in% cols) return(candidate)
  n_cols <- tolower(normalize_name(cols)); n_cand <- tolower(normalize_name(candidate))
  hit <- which(n_cols == n_cand); if (length(hit)) return(cols[hit[1]])
  n_cands <- tolower(normalize_name(c(candidate, key_candidates)))
  hit2 <- which(n_cols %in% n_cands); if (length(hit2)) return(cols[hit2[1]])
  NA_character_
}
as_cat_objname <- function(table_name) paste0("cat_", make.names(gsub("\\s+","_", table_name)))

# ---------- 1) Read RelCatalog (XLSX) ----------
if (!file.exists(relcatalog_xlsx)) stop("RelCatalog XLSX not found: ", relcatalog_xlsx)
rel_raw <- tryCatch(read.xlsx(relcatalog_xlsx, sheet = relcatalog_sheet), error = function(e) NULL)
if (is.null(rel_raw) || !nrow(rel_raw)) stop("RelCatalog XLSX is empty or unreadable.")

# Your columns: Object / ColumnName / RefObject / RefColumn / RelationshipName / RelAttributes  [1](https://007gc-my.sharepoint.com/personal/francois_bolduc_ec_gc_ca/_layouts/15/Doc.aspx?sourcedoc=%7BB13A33B6-ACD8-4E88-9B51-3F3D5A0E7D38%7D&file=RelCatalog.xlsx&action=default&mobileredirect=true)
rn <- tolower(names(rel_raw))
relcatalog_df <- tibble(
  Object      = normalize_name(rel_raw[[which.max(rn %in% c("object","table","objet"))]]),
  ColumnName  = normalize_name(rel_raw[[which.max(rn %in% c("columnname","column","champ","field"))]]),
  RefObject   = normalize_name(rel_raw[[which.max(rn %in% c("refobject","foreign_table","reftable","refobjet"))]]),
  RefColumn   = normalize_name(rel_raw[[which.max(rn %in% c("refcolumn","foreigncolumn","reffield","refcolonne"))]])
) %>%
  filter(!is.na(Object), !is.na(ColumnName), !is.na(RefObject), !is.na(RefColumn))

assign("relcatalog_df", relcatalog_df, envir = .GlobalEnv)
message("RelCatalog rows: ", nrow(relcatalog_df))

# ---------- 2) Connect to Access and list tables ----------
if (!file.exists(accdb_path)) stop("Access DB not found: ", accdb_path)
message("Connecting to Access via ODBC…")
con <- odbcConnectAccess2007(accdb_path, believeNRows = FALSE)
on.exit(odbcClose(con), add = TRUE)
acc_tables <- sqlTables(con)$TABLE_NAME

# ---------- 3) Identify catalog-vs-data per row & load catalog tables ----------
# In your file, Catalog=Object / Data=RefObject (ignore Data↔Data rows)  [1](https://007gc-my.sharepoint.com/personal/francois_bolduc_ec_gc_ca/_layouts/15/Doc.aspx?sourcedoc=%7BB13A33B6-ACD8-4E88-9B51-3F3D5A0E7D38%7D&file=RelCatalog.xlsx&action=default&mobileredirect=true)
data_tables <- c("missions","transects","observations")
rows <- relcatalog_df %>%
  mutate(
    obj_is_data = tolower(Object) %in% data_tables,
    ref_is_data = tolower(RefObject) %in% data_tables,
    # decide direction; if both or neither are data => not a catalog validation row
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

# Load each unique catalog table once and resolve its key column per row
catalog_map_df <- rows %>%
  distinct(catalog_table) %>%
  mutate(access_table = vapply(catalog_table, resolve_access_table, character(1), all_names = acc_tables),
         found_in_access = !is.na(access_table))

# Fetch and create cat_* objects
for (i in seq_len(nrow(catalog_map_df))) {
  ct <- catalog_map_df$catalog_table[i]
  at <- catalog_map_df$access_table[i]
  if (is.na(at)) {
    message("  - Catalog table not found in Access: ", ct)
    next
  }
  df <- tryCatch(sqlFetch(con, at), error = function(e) NULL)
  if (is.null(df)) {
    message("  - Could not fetch catalog table: ", at)
    next
  }
  obj <- as_cat_objname(ct)
  if (!exists(obj, envir = .GlobalEnv)) {
    assign(obj, df, envir = .GlobalEnv)
    message("  - Loaded catalog: ", ct, "  ->  ", obj, " [", nrow(df), " x ", ncol(df), "]")
  }
}

# Resolve catalog column for each row, and produce final tidy mapping
catalog_map_df <- rows %>%
  mutate(
    cat_obj = vapply(catalog_table, function(ct) {
      obj <- as_cat_objname(ct)
      if (exists(obj, envir = .GlobalEnv)) obj else NA_character_
    }, character(1)),
    catalog_column_resolved = mapply(function(obj, col) {
      if (is.na(obj)) return(NA_character_)
      resolve_col_name(col, names(get(obj, envir = .GlobalEnv)))
    }, cat_obj, catalog_column),
    ok = !is.na(cat_obj) & !is.na(catalog_column_resolved)
  ) %>%
  transmute(
    data_table, data_column,
    catalog_table, catalog_column = catalog_column_resolved,
    cat_obj, ok
  )

assign("catalog_map_df", catalog_map_df, envir = .GlobalEnv)

# Diagnostic: which rows failed to resolve (table or column)?
relcatalog_diag_df <- catalog_map_df %>%
  mutate(
    table_found = !is.na(cat_obj),
    refcolumn_found = !is.na(catalog_column)
  )
assign("relcatalog_diag_df", relcatalog_diag_df, envir = .GlobalEnv)

message("Catalog rows (candidate): ", nrow(rows),
        " | resolved mappings: ", sum(catalog_map_df$ok, na.rm = TRUE))

# Convenience: list the loaded cat_* objects
list_loaded_catalogs <- function() {
  objs <- ls(pattern = "^cat_", envir = .GlobalEnv)
  if (!length(objs)) return(cat("No cat_* objects loaded.\n"))
  cat("Catalog data frames:\n")
  for (nm in objs) {
    df <- get(nm, envir = .GlobalEnv)
    cat(sprintf("  %s [%d x %d]\n", nm, nrow(df), ncol(df)))
  }
}
assign("list_loaded_catalogs", list_loaded_catalogs, envir = .GlobalEnv)

# Function to build a named list mapping "table$col" -> allowed vector (EXACT values)
build_catalog_map <- function() {
  mp <- list()
  for (i in seq_len(nrow(catalog_map_df))) {
    if (!isTRUE(catalog_map_df$ok[i])) next
    key <- paste0(tolower(catalog_map_df$data_table[i]), "$", tolower(catalog_map_df$data_column[i]))
    obj <- catalog_map_df$cat_obj[i]
    col <- catalog_map_df$catalog_column[i]
    vals <- unique(na.omit(as.character(get(obj, envir = .GlobalEnv)[[col]])))
    mp[[key]] <- vals
  }
  mp
}
assign("build_catalog_map", build_catalog_map, envir = .GlobalEnv)

message("=== Catalog Loader complete ===")
message("Objects now in your environment:")
message("  - relcatalog_df, catalog_map_df, relcatalog_diag_df")
message("  - cat_* for each catalog table loaded")
message("  - list_loaded_catalogs(), build_catalog_map()")