# === SOMEC Mission Profiler v6.3.4 ===
# QA/QC reporting for mission data with platform-specific cross-validation

# === CONFIG ===================================================================
cfg <- list(
  force_rebuild = FALSE,        # TRUE = rebuild all Excel reports + mission_issues
  force_rebuild_issues = TRUE,  # TRUE = rebuild mission_issues only (no Excel rewrite)
  base_dir = project_dir,       # root folder for output
  out_folder = 'MissionReports',
  relcatalog_xlsx  = normalizePath(file.path(project_dir, 'RelCatalog.xlsx'), winslash = '/', mustWork = FALSE),
  relcatalog_sheet = 'RelCatalog',
  accdb_path       = resolve_accdb_path(project_dir),

  rare_threshold_pct = 0.1,
  max_levels_show = 20,
  top_unknowns_per_col = 20,
  outlier_rule = 'tukey',
  skip_pattern = '(comment|commentaire|note|remarque)',
  img_width = 3.6, img_height = 2.2,
  plot_theme = theme_minimal(base_size = 8),
  tile_cols = 2, tile_width_cols = 9,
  tile_height_rows = list(numeric = 16, categorical = 26, datetime = 14),
  write_catalog_artifacts = TRUE
)

# === Catalog prerequisites ===================================================
.need_funs <- c('build_catalog_map')
.need_objs <- c('catalog_map_df', 'relcatalog_df')
.have_cats <- grepl('^cat_', ls(envir = .GlobalEnv))

.missing_funs <- .need_funs[!vapply(.need_funs, exists, logical(1), mode = 'function')]
.missing_objs <- .need_objs[!vapply(.need_objs, exists, logical(1), inherits = FALSE)]

mp <- build_catalog_map()
message('Catalog map ready: ', length(mp), ' field(s).')

out_dir <- file.path(cfg$base_dir, cfg$out_folder)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

message('Profiler start: ', format(Sys.time()))
message('Output folder: ', out_dir)

# === Load main data (auto-load from Access if missing) =======================
auto_load <- function(tbl, accdb, acc_name) {
  if (exists(tbl, envir = .GlobalEnv)) return(invisible(TRUE))
  if (is.null(accdb) || !nzchar(accdb))
    stop('ACCDB path is not set. Check resolve_accdb_path().', call. = FALSE)
  if (!file.exists(accdb))
    stop('ACCDB file not found: ', accdb, call. = FALSE)
  con <- RODBC::odbcConnectAccess2007(accdb, believeNRows = FALSE)
  if (identical(con, -1L))
    stop('ODBC connection failed for: ', accdb, call. = FALSE)
  on.exit(RODBC::odbcClose(con), add = TRUE)
  df <- tryCatch(RODBC::sqlFetch(con, acc_name), error = function(e) NULL)
  if (is.null(df)) stop('Could not load table from Access: ', acc_name)
  assign(tbl, df, envir = .GlobalEnv)
  message('Loaded data: ', tbl, ' [', nrow(df), ' x ', ncol(df), ']')
  invisible(TRUE)
}

auto_load('missions',     cfg$accdb_path, 'missions')
auto_load('transects',    cfg$accdb_path, 'transects')
auto_load('observations', cfg$accdb_path, 'observations')

missions_df <- missions %>% distinct(mission, .keep_all = TRUE)
message('Missions found: ', nrow(missions_df))

mf <- getOption('somec.mission_filter', default = NULL)
if (!is.null(mf) && length(mf)) {
  missions_df <- missions_df %>% dplyr::filter(.data$mission %in% mf)
}

# === Helpers ==================================================================
is_comment_col <- function(nm) grepl(cfg$skip_pattern, nm, ignore.case = TRUE)
is_dt <- function(x) inherits(x, 'POSIXt') || inherits(x, 'Date')

numeric_summary <- function(x) {
  xx <- x[is.finite(x)]
  if (!length(xx)) return(tibble(n = 0))
  q <- quantile(xx, c(.05,.25,.5,.75,.95), names = FALSE); iqr <- q[4] - q[2]
  tibble(n = length(xx), n_missing = sum(!is.finite(x)),
         min = min(xx), p05 = q[1], p25 = q[2], median = q[3], p75 = q[4], p95 = q[5], max = max(xx),
         iqr = iqr,
         outlier_low  = q[2] - 1.5*iqr,
         outlier_high = q[4] + 1.5*iqr)
}

freq_table <- function(x, colname, max_rows = cfg$max_levels_show) {
  tibble(value = as.character(x)) |>
    mutate(value = ifelse(is.na(value) | value == 'NA', NA_character_, value)) |>
    count(value, sort = TRUE, name = 'n') |>
    mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 2),
           rare = pct < cfg$rare_threshold_pct) |>
    slice_head(n = max_rows)
}

plot_hist <- function(x, title='', xlab='') {
  df <- tibble(x = x) |> filter(is.finite(x))
  if (!nrow(df)) return(NULL)
  ggplot(df, aes(x)) + geom_histogram(color='grey30', fill='#4C78A8', bins=30) +
    labs(title=title, x=xlab, y='N') + cfg$plot_theme
}

plot_hour_hist <- function(dt, title) {
  if (all(is.na(dt))) return(NULL)
  hh <- hour(as_datetime(dt)); df <- tibble(h = hh)
  ggplot(df, aes(h)) + geom_histogram(binwidth=1, boundary=0, color='grey30', fill='#59A14F') +
    scale_x_continuous(breaks=0:23) +
    labs(title=title, x='Heure (0–23)', y='N') + cfg$plot_theme
}

insert_plot <- function(wb, sheet, plot_obj, startRow, startCol) {
  if (is.null(plot_obj)) return(invisible(NULL))
  tf <- tempfile(fileext = '.png'); ggsave(tf, plot=plot_obj, width=cfg$img_width, height=cfg$img_height, dpi=150)
  insertImage(wb, sheet, tf, startRow=startRow, startCol=startCol, width=cfg$img_width, height=cfg$img_height, units='in')
}

place_tile <- function(wb, sheet, startRow, startCol, title, table_df=NULL, plot_obj=NULL) {
  writeData(wb, sheet, title, startRow=startRow, startCol=startCol)
  addStyle(wb, sheet, createStyle(textDecoration='bold'), rows=startRow, cols=startCol)
  if (!is.null(table_df)) writeData(wb, sheet, table_df, startRow=startRow+1, startCol=startCol)
  if (!is.null(plot_obj)) insert_plot(wb, sheet, plot_obj, startRow=startRow+1, startCol=startCol+6)
}

tile_position <- function(tile_index, tile_height, tile_width) {
  row_block <- (tile_index - 1) %/% cfg$tile_cols
  col_block <- (tile_index - 1) %%  cfg$tile_cols
  list(row = 2 + row_block * tile_height, col = 1 + col_block * tile_width)
}

# === write_one_sheet function ================================================
write_one_sheet <- function(wb, sheet_name, df, mission_id, mis_start, mis_end, table_name, qc_summary, platform_type = NA_character_, dry_run = FALSE) {
  if (!nrow(df)) return(qc_summary)
  df <- df[, !sapply(names(df), is_comment_col), drop = FALSE]
  is_datetime <- sapply(df, is_dt)
  has_catalog <- function(col) { key <- paste0(tolower(table_name), '$', tolower(col)); !is.null(mp[[key]]) }
  force_categorical_names <- c('mission', 'cote_obs', 'code_obs')
  nm <- names(df); nm_lc <- tolower(nm)
  is_forced_cat <- nm_lc %in% force_categorical_names
  is_categorical <- vapply(nm, has_catalog, logical(1)) | is_forced_cat
  is_numeric <- !is_categorical & !is_datetime
  cat_cols <- nm[is_categorical]; num_cols <- nm[is_numeric]; dt_cols <- nm[is_datetime]
  START_COL <- 1; ROW_GAP <- 1; PLOT_ROWS <- 12; row_cur <- 2
  place_block <- function(title, tbl, plot_obj = NULL) {
    if (!dry_run) {
      writeData(wb, sheet_name, title, startRow = row_cur, startCol = START_COL)
      addStyle(wb, sheet_name, createStyle(textDecoration = 'bold'), rows = row_cur, cols = START_COL)
      writeData(wb, sheet_name, tbl, startRow = row_cur + 1, startCol = START_COL)
      if (!is.null(plot_obj)) {
        plot_col <- START_COL + ncol(tbl) + 3
        insert_plot(wb, sheet_name, plot_obj, startRow = row_cur + 1, startCol = plot_col)
      }
    }
    used_rows <- nrow(tbl) + 2
    row_cur <<- row_cur + max(used_rows, PLOT_ROWS) + ROW_GAP
  }
  for (col in num_cols) {
    x_raw <- df[[col]]; x_num <- suppressWarnings(as.numeric(x_raw))
    s <- tibble(n = sum(!is.na(x_num)), n_missing = sum(is.na(x_num)))
    stats <- numeric_summary(x_num)
    if (stats$n > 0) {
      is_low <- x_num < stats$outlier_low; is_high <- x_num > stats$outlier_high
      n_low <- sum(is_low, na.rm = TRUE); n_high <- sum(is_high, na.rm = TRUE)
      if (n_low + n_high > 0) {
        qc_summary$numeric_outliers <- add_row(qc_summary$numeric_outliers, table = table_name, column = col, n_low = n_low, n_high = n_high)
        mission_issues <<- add_row(mission_issues, mission = mission_id, table = table_name, column = col, issue_type = 'NUMERIC_OUTLIER', platform_type = platform_type, details = paste(n_low, 'below IQR-low;', n_high, 'above IQR-high'))
      }
    }
    if (tolower(col) %in% c('dir_vent', 'dir_plateforme')) {
      bad_dir <- x_num[is.finite(x_num) & x_num != 999 & (x_num < 0 | x_num > 360)]
      if (length(bad_dir) > 0) {
        mission_issues <<- add_row(mission_issues, mission = mission_id, table = table_name, column = col, issue_type = 'DIRECTION_RANGE', platform_type = platform_type, details = paste(length(bad_dir), 'value(s) outside [0, 360]:', paste(head(sort(unique(bad_dir)), 5), collapse = ', ')))
      }
    }
    if (tolower(col) == 'longitude') {
      n_pos <- sum(x_num > 0, na.rm = TRUE)
      if (n_pos > 0) {
        mission_issues <<- add_row(mission_issues, mission = mission_id, table = table_name, column = col, issue_type = 'LONGITUDE_SIGN', platform_type = platform_type, details = paste(n_pos, 'record(s) with positive longitude'))
      }
    }
    p <- if (!dry_run && s$n[1] > 0) plot_hist(x_num, title = col, xlab = col) else NULL
    place_block(title = paste0('• ', col, ' (num)'), tbl = s, plot_obj = p)
  }
  for (col in cat_cols) {
    v <- df[[col]]; ft <- freq_table(v, col, cfg$max_levels_show)
    found_hdr <- 'found in the catalog'; ft[[found_hdr]] <- NA
    key <- paste0(tolower(table_name), '$', tolower(col)); unknowns_tbl <- NULL
    if (!is.null(mp[[key]])) {
      allowed <- mp[[key]]; vals <- as.character(v); is_found <- vals %in% allowed
      ft[[found_hdr]] <- ft$value %in% allowed
      is_unknown <- !is_found & !is.na(vals)
      if (any(is_unknown)) {
        unknowns_tbl <- tibble(value = vals[is_unknown]) |> count(value, sort = TRUE, name = 'n_records') |> slice_head(n = cfg$top_unknowns_per_col) |> mutate(table = table_name, column = col, .before = 1)
        mission_issues <<- add_row(mission_issues, mission = mission_id, table = table_name, column = col, issue_type = 'UNKNOWN_CATALOG', platform_type = platform_type, details = paste(unique(vals[is_unknown]), collapse = ', '))
      }
      qc_summary$catalog_unknowns <- add_row(qc_summary$catalog_unknowns, table = table_name, column = col, n_unknown_levels = sum(ft[[found_hdr]] == FALSE, na.rm = TRUE), n_unknown_records = sum(is_unknown))
    } else {
      qc_summary$catalog_unknowns <- add_row(qc_summary$catalog_unknowns, table = table_name, column = col, n_unknown_levels = NA_integer_, n_unknown_records = NA_integer_)
    }
    place_block(title = paste0('• ', col, ' (categorical)'), tbl = ft, plot_obj = NULL)
    if (!dry_run) {
      bad_idx <- which(ft[[found_hdr]] == FALSE)
      if (length(bad_idx)) {
        start_of_block <- row_cur - max(nrow(ft) + 2, PLOT_ROWS) - ROW_GAP
        xl_rows <- start_of_block + 1 + bad_idx
        conditionalFormatting(wb, sheet_name, cols = START_COL:(START_COL + ncol(ft) - 1), rows = xl_rows, type = 'expression', rule = 'TRUE', style = createStyle(fontColour = 'red'))
      }
    }
    if (!is.null(unknowns_tbl) && nrow(unknowns_tbl)) { qc_summary$top_unknowns[[length(qc_summary$top_unknowns) + 1]] <- unknowns_tbl }
  }
  for (col in dt_cols) {
    dt <- suppressWarnings(as_datetime(df[[col]])); dmin <- suppressWarnings(min(dt, na.rm = TRUE)); dmax <- suppressWarnings(max(dt, na.rm = TRUE))
    n_out <- if (!is.na(mis_start) && !is.na(mis_end)) sum(dt < mis_start | dt > mis_end, na.rm = TRUE) else NA_integer_
    tbl <- tibble(n = sum(!is.na(dt)), min = as.character(dmin), max = as.character(dmax), n_outside_mission = n_out)
    p <- if (!dry_run) plot_hour_hist(dt, title = paste0(col, ' – heure')) else NULL
    place_block(title = paste0('• ', col, ' (datetime)'), tbl = tbl, plot_obj = p)
    if (!is.na(n_out) && n_out > 0) {
      qc_summary$mission_bounds <- add_row(qc_summary$mission_bounds, table = table_name, column = col, n_outside = n_out)
      mission_issues <<- add_row(mission_issues, mission = mission_id, table = table_name, column = col, issue_type = 'OUTSIDE_MISSION_DATES', platform_type = platform_type, details = paste(n_out, 'records outside mission period'))
    }
  }
  return(qc_summary)
}

# === check_platform_rules ===================================================
check_platform_rules <- function(t_df, o_df, mission_id, platform_type) {
  flag <- function(table, col, details) {
    mission_issues <<- add_row(mission_issues, mission = mission_id, table = table, column = col, issue_type = 'PLATFORM_CROSS_CHECK', platform_type = platform_type, details = details)
  }
  if (platform_type == 'plane') {
    for (col in c('hau_yeux', 'instantane')) {
      if (col %in% names(t_df)) {
        v <- suppressWarnings(as.numeric(t_df[[col]]))
        n_bad <- sum(is.finite(v) & v != 999, na.rm = TRUE)
        if (n_bad > 0) flag('transects', col, paste0(n_bad, ' non-sentinel value(s) on plane'))
      }
    }
    if ('int_exterieur' %in% names(t_df)) {
      n_bad <- sum(!is.na(t_df$int_exterieur))
      if (n_bad > 0) flag('transects', 'int_exterieur', paste0(n_bad, ' non-NA value(s) on plane'))
    }
    if ('snapshot' %in% names(o_df)) {
      n_bad <- sum(o_df$snapshot != 'Non', na.rm = TRUE)
      if (n_bad > 0) flag('observations', 'snapshot', paste0(n_bad, ' records != Non on plane'))
    }
    if ('dis_par' %in% names(o_df)) {
      allowed_codes <- c('J', 'K', 'L', 'M', 'N', 'O')
      bad <- o_df$dis_par[!is.na(o_df$dis_par) & !(o_df$dis_par %in% allowed_codes)]
      if (length(bad) > 0) { n_bad <- length(bad); flag('observations', 'dis_par', paste0(n_bad, ' non-plane dis_par code(s): ', paste(unique(bad), collapse = ', '))) }
    }
  } else {
    if ('hau_yeux' %in% names(t_df) && nrow(t_df) > 0) {
      v <- suppressWarnings(as.numeric(t_df$hau_yeux)); pct_999 <- mean(v == 999, na.rm = TRUE)
      if (isTRUE(pct_999 == 1)) flag('transects', 'hau_yeux', 'all values are 999 on ship mission')
    }
    if ('instantane' %in% names(t_df) && nrow(t_df) > 0) {
      v <- suppressWarnings(as.numeric(t_df$instantane)); pct_999 <- mean(v == 999, na.rm = TRUE)
      if (isTRUE(pct_999 == 1)) flag('transects', 'instantane', 'all values are 999 on ship mission')
    }
    if ('snapshot' %in% names(o_df) && nrow(o_df) > 0) {
      pct_non <- mean(o_df$snapshot == 'Non', na.rm = TRUE)
      if (isTRUE(pct_non == 1)) flag('observations', 'snapshot', 'all values are Non on ship mission')
    }
    if ('dis_par' %in% names(o_df)) {
      allowed_codes <- c('3', 'A', 'B', 'C', 'D', 'E', 'F', 'T', 'Z')
      bad <- o_df$dis_par[!is.na(o_df$dis_par) & !(o_df$dis_par %in% allowed_codes)]
      if (length(bad) > 0) { n_bad <- length(bad); flag('observations', 'dis_par', paste0(n_bad, ' non-ship dis_par code(s): ', paste(unique(bad), collapse = ', '))) }
    }
  }
}
# R10 (watch speed/distance CV) is handled by run_cross_validation() in qc_cross_validation.R

# === MAIN LOOP
.plane_labels <- c('Avion', 'Islander', 'Partenavia')
mission_issues <- tibble(mission = character(), table = character(), column = character(), issue_type = character(), platform_type = character(), details = character())

all_index <- list()
for (i in seq_len(nrow(missions_df))) {
  mis_id <- missions_df$mission[i]
  out_file <- file.path(out_dir, paste0(mis_id, '.xlsx'))
  m_row <- missions %>% filter(mission == mis_id) %>% slice_head(n = 1)
  t_df <- transects %>% filter(mission == mis_id)
  o_df <- observations %>% filter(mission == mis_id)
  mis_start <- suppressWarnings(as_datetime(m_row$debut))
  mis_end <- suppressWarnings(as_datetime(m_row$fin))
  plt_type <- if_else(m_row$nom_plateforme %in% .plane_labels, 'plane', 'ship', missing = 'ship')
  
  if (!cfg$force_rebuild && cfg$force_rebuild_issues && file.exists(out_file)) {
    message(sprintf('[%d/%d] Mission %s - issues-only (skipping Excel)', i, nrow(missions_df), mis_id))
    dummy_qc <- list(numeric_outliers = tibble(table=character(), column=character(), n_low=integer(), n_high=integer()), rare_categories = tibble(table=character(), column=character(), n_rare_levels=integer()), mission_bounds = tibble(table=character(), column=character(), n_outside=integer()), catalog_unknowns = tibble(table=character(), column=character(), n_unknown_levels=integer(), n_unknown_records=integer()), top_unknowns = list())
    write_one_sheet(NULL, 'Transects', t_df, mis_id, mis_start, mis_end, 'transects', dummy_qc, plt_type, dry_run = TRUE)
    write_one_sheet(NULL, 'Observations', o_df, mis_id, mis_start, mis_end, 'observations', dummy_qc, plt_type, dry_run = TRUE)
    check_platform_rules(t_df, o_df, mis_id, plt_type)
    # R10 fired interactively via run_cross_validation() in qc_cross_validation.R
    all_index[[length(all_index) + 1]] <- tibble(mission = mis_id, n_transects = nrow(t_df), n_observations = nrow(o_df), dt_mission_start = as.character(mis_start), dt_mission_end = as.character(mis_end), report_path = out_file)
    message(sprintf('[%d/%d] Mission %s - done (issues-only)', i, nrow(missions_df), mis_id))
    next
  }
  
  if (!cfg$force_rebuild && file.exists(out_file)) {
    message(sprintf('[%d/%d] Mission %s - report exists, skipping', i, nrow(missions_df), mis_id))
    next
  }
  
  message(sprintf('[%d/%d] Mission %s - start', i, nrow(missions_df), mis_id))
  wb <- createWorkbook()
  addWorksheet(wb, 'Missions'); addWorksheet(wb, 'Transects'); addWorksheet(wb, 'Observations'); addWorksheet(wb, 'QC_Summary')
  setColWidths(wb, 'Transects', cols = 1:(cfg$tile_cols * cfg$tile_width_cols + 2), widths = 12)
  setColWidths(wb, 'Observations', cols = 1:(cfg$tile_cols * cfg$tile_width_cols + 2), widths = 12)
  writeData(wb, 'Missions', paste0('Mission: ', mis_id), startRow = 1, startCol = 1)
  addStyle(wb, 'Missions', createStyle(textDecoration = 'bold', fontSize = 12), rows = 1, cols = 1)
  writeData(wb, 'Missions', m_row, startRow = 3, startCol = 1)
  sizes <- tibble(n_transects = nrow(t_df), n_observations = nrow(o_df), obs_dt_min = if (nrow(o_df)) as.character(suppressWarnings(min(as_datetime(o_df$date_heure), na.rm = TRUE))) else NA, obs_dt_max = if (nrow(o_df)) as.character(suppressWarnings(max(as_datetime(o_df$date_heure), na.rm = TRUE))) else NA)
  writeData(wb, 'Missions', sizes, startRow = 5 + nrow(m_row), startCol = 1)
  
  qc_summary <- list(numeric_outliers = tibble(table=character(), column=character(), n_low=integer(), n_high=integer()), rare_categories = tibble(table=character(), column=character(), n_rare_levels=integer()), mission_bounds = tibble(table=character(), column=character(), n_outside=integer()), catalog_unknowns = tibble(table=character(), column=character(), n_unknown_levels=integer(), n_unknown_records=integer()), top_unknowns = list())
  
  qc_summary <- write_one_sheet(wb, 'Transects', t_df, mis_id, mis_start, mis_end, 'transects', qc_summary, plt_type)
  qc_summary <- write_one_sheet(wb, 'Observations', o_df, mis_id, mis_start, mis_end, 'observations', qc_summary, plt_type)
  check_platform_rules(t_df, o_df, mis_id, plt_type)
  
  qc_df <- bind_rows(qc_summary$numeric_outliers %>% mutate(type = 'numeric_outliers'), qc_summary$rare_categories %>% mutate(type = 'rare_categories'), qc_summary$mission_bounds %>% mutate(type = 'mission_date_bounds'), qc_summary$catalog_unknowns %>% mutate(type = 'catalog_unknowns'))
  if (nrow(qc_df)) { writeData(wb, 'QC_Summary', qc_df, startRow = 1, startCol = 1) } else { writeData(wb, 'QC_Summary', tibble(note = 'No summary flags'), startRow = 1, startCol = 1) }
  if (length(qc_summary$top_unknowns)) {
    tu <- bind_rows(qc_summary$top_unknowns)
    writeData(wb, 'QC_Summary', 'TOP UNKNOWN LEVELS (frequency only)', startRow = nrow(qc_df) + 3, startCol = 1)
    addStyle(wb, 'QC_Summary', createStyle(textDecoration = 'bold'), rows = nrow(qc_df) + 3, cols = 1)
    writeData(wb, 'QC_Summary', tu, startRow = nrow(qc_df) + 4, startCol = 1)
  }
  
  message('  - Saving: ', out_file)
  saveWorkbook(wb, out_file, overwrite = TRUE)
  all_index[[length(all_index) + 1]] <- tibble(mission = mis_id, n_transects = nrow(t_df), n_observations = nrow(o_df), dt_mission_start = as.character(mis_start), dt_mission_end = as.character(mis_end), report_path = out_file)
  message(sprintf('[%d/%d] Mission %s - done', i, nrow(missions_df), mis_id))
}

# === OUTPUT ================================================================
idx <- bind_rows(all_index)
wb_idx <- createWorkbook()
addWorksheet(wb_idx, 'Index')
writeData(wb_idx, 'Index', idx, startRow = 1, startCol = 1)
idx_path <- file.path(out_dir, paste0('SOMEC_Mission_QAQC_Index_', format(Sys.Date(), '%Y%m%d'), '.xlsx'))
saveWorkbook(wb_idx, idx_path, overwrite = TRUE)
message('Profiler complete: ', idx_path)

mi_path <- file.path(out_dir, 'mission_issues.rds')
if (!cfg$force_rebuild && !cfg$force_rebuild_issues && file.exists(mi_path)) {
  message('mission_issues.rds exists - skipping save.')
} else {
  saveRDS(mission_issues, mi_path)
  message('mission_issues.rds saved: ', nrow(mission_issues), ' issue(s).')
}
