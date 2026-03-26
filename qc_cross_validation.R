# SOMEC cross-validation rules for the QC console.
# Sourced by interactive_mission_qc_console.R

.rad_par_both_missions <- c("HUD081026", "MLB090423", "HUD091025", "TEL100603", "HUD101107")

.rien_cols <- c(
  "nb_individu", "activite", "tra_rad", "dis_rad", "tra_par", "dis_par",
  "association", "comportement", "direction", "dir_heure", "age", "plumage", "sexe"
)

normalize_obs <- function(df) {
  df |>
    dplyr::mutate(
      activite_n = stringr::str_to_upper(stringr::str_trim(activite)),
      snapshot_n = stringr::str_to_upper(stringr::str_trim(snapshot)),
      in_transect_n = stringr::str_to_upper(stringr::str_trim(in_transect)),
      espece_n = stringr::str_to_upper(stringr::str_trim(code_espece))
    )
}

run_cross_validation <- function(mission_id) {
  obs_mis <- observations |>
    tibble::as_tibble() |>
    dplyr::filter(.data[["mission"]] == mission_id) |>
    normalize_obs()

  is_both_mission <- mission_id %in% .rad_par_both_missions
  results <- list()

  r1 <- obs_mis |>
    dplyr::filter(is.na(id_transect), in_transect_n != "HORS TRANSECT") |>
    dplyr::select(dplyr::any_of(c("id", "date", "heure", "in_transect", "code_espece", "id_transect")))
  if (nrow(r1) > 0) results$R1 <- r1

  r2 <- obs_mis |>
    dplyr::filter(espece_n == "RIEN") |>
    dplyr::filter(dplyr::if_any(dplyr::any_of(.rien_cols), ~ !is.na(.))) |>
    dplyr::select(dplyr::any_of(c("id", "date", "heure", "code_espece", .rien_cols)))
  if (nrow(r2) > 0) results$R2 <- r2

  r3 <- obs_mis |>
    dplyr::filter(espece_n != "RIEN") |>
    dplyr::filter(is.na(nb_individu) | is.na(activite)) |>
    dplyr::select(dplyr::any_of(c("id", "date", "heure", "code_espece", "nb_individu", "activite", "snapshot", "in_transect")))
  if (nrow(r3) > 0) results$R3 <- r3

  r4_base <- obs_mis |> dplyr::filter(espece_n != "RIEN")
  r4 <- if (is_both_mission) {
    r4_base |> dplyr::filter(is.na(tra_rad) & is.na(dis_rad) & is.na(tra_par) & is.na(dis_par))
  } else {
    r4_base |> dplyr::filter(is.na(tra_par) & is.na(dis_par))
  }
  r4 <- r4 |>
    dplyr::select(dplyr::any_of(c("id", "date", "heure", "code_espece", "activite", "tra_rad", "dis_rad", "tra_par", "dis_par")))
  if (nrow(r4) > 0) results$R4 <- r4

  if (!is_both_mission) {
    r5 <- obs_mis |>
      dplyr::filter(espece_n != "RIEN") |>
      dplyr::filter(!is.na(tra_rad) | !is.na(dis_rad)) |>
      dplyr::select(dplyr::any_of(c("id", "date", "heure", "code_espece", "tra_rad", "dis_rad", "tra_par", "dis_par")))
    if (nrow(r5) > 0) results$R5 <- r5
  }

  r6 <- obs_mis |>
    dplyr::filter(espece_n != "RIEN", activite_n == "VOL", snapshot_n != "OUI") |>
    dplyr::select(dplyr::any_of(c("id", "date", "heure", "code_espece", "activite", "snapshot", "activite_n", "snapshot_n")))
  if (nrow(r6) > 0) results$R6 <- r6

  r7 <- obs_mis |>
    dplyr::filter(espece_n != "RIEN", activite_n == "EAU", snapshot_n != "NON") |>
    dplyr::select(dplyr::any_of(c("id", "date", "heure", "code_espece", "activite", "snapshot", "activite_n", "snapshot_n")))
  if (nrow(r7) > 0) results$R7 <- r7

  r8 <- obs_mis |>
    dplyr::filter(espece_n != "RIEN") |>
    dplyr::filter(is.na(in_transect) | is.na(snapshot)) |>
    dplyr::select(dplyr::any_of(c("id", "date", "heure", "code_espece", "activite", "snapshot", "in_transect")))
  if (nrow(r8) > 0) results$R8 <- r8

  if (is_both_mission) {
    r9 <- obs_mis |>
      dplyr::filter(espece_n != "RIEN") |>
      dplyr::filter(!is.na(tra_rad) & !is.na(tra_par) & tra_rad != tra_par) |>
      dplyr::select(dplyr::any_of(c("id", "date", "heure", "code_espece", "activite", "tra_rad", "dis_rad", "tra_par", "dis_par")))
    if (nrow(r9) > 0) results$R9 <- r9
  }

  .plane_pattern <- "^(AVI|PAR|ISL)"
  if (!grepl(.plane_pattern, mission_id)) {
    t_mis <- transects |>
      tibble::as_tibble() |>
      dplyr::filter(.data[["mission"]] == mission_id)

    req_cols <- c("id_transect", "site", "longitude", "latitude", "date_heure", "vit_plateforme")
    if (all(req_cols %in% names(t_mis))) {
      norm_site <- function(x) {
        y <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT"))
        dplyr::case_when(
          y %in% c("debut", "start") ~ "debut",
          y %in% c("fin", "end") ~ "fin",
          TRUE ~ NA_character_
        )
      }

      vit_stats <- t_mis |>
        dplyr::filter(!is.na(vit_plateforme)) |>
        dplyr::group_by(id_transect) |>
        dplyr::summarise(
          vit_min_kn = min(vit_plateforme, na.rm = TRUE),
          vit_max_kn = max(vit_plateforme, na.rm = TRUE),
          vit_med_kn = stats::median(vit_plateforme, na.rm = TRUE),
          .groups = "drop"
        )

      w <- t_mis |>
        dplyr::mutate(site_norm = norm_site(site)) |>
        dplyr::filter(site_norm %in% c("debut", "fin")) |>
        dplyr::group_by(id_transect, site_norm) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup() |>
        dplyr::select(id_transect, site_norm, longitude, latitude, date_heure) |>
        tidyr::pivot_wider(names_from = site_norm, values_from = c(longitude, latitude, date_heure), names_sep = "_") |>
        dplyr::mutate(
          dt_debut = suppressWarnings(lubridate::as_datetime(date_heure_debut)),
          dt_fin = suppressWarnings(lubridate::as_datetime(date_heure_fin)),
          WatchLenKm = geosphere::distHaversine(cbind(longitude_debut, latitude_debut), cbind(longitude_fin, latitude_fin)) / 1000,
          Duration_h = as.numeric(difftime(dt_fin, dt_debut, units = "hours")),
          Observed_kmh = dplyr::if_else(!is.na(Duration_h) & Duration_h > 0, WatchLenKm / Duration_h, NA_real_)
        ) |>
        dplyr::left_join(vit_stats, by = "id_transect") |>
        dplyr::mutate(
          vit_min_kmh = vit_min_kn * 1.852,
          vit_max_kmh = vit_max_kn * 1.852,
          Vit_knots2kmh = vit_med_kn * 1.852,
          in_speed_range = !is.na(Observed_kmh) & !is.na(vit_min_kmh) & !is.na(vit_max_kmh) &
            Observed_kmh >= (vit_min_kmh * 0.9) & Observed_kmh <= (vit_max_kmh * 1.1),
          pct_diff = (Observed_kmh - Vit_knots2kmh) / Vit_knots2kmh * 100,
          flag = (!in_speed_range & abs(pct_diff) > 20) | (!is.na(Duration_h) & Duration_h < 0) | is.na(Vit_knots2kmh) | is.na(Observed_kmh),
          reason = dplyr::case_when(
            !is.na(Duration_h) & Duration_h < 0 ~ "duree_negative",
            is.na(Vit_knots2kmh) ~ "vit_plateforme_manquante",
            !is.na(vit_med_kn) & vit_med_kn <= 3 ~ "vit_tres_faible",
            !is.na(Duration_h) & Duration_h > 0 & Duration_h < 0.1 ~ "duree_tres_courte",
            TRUE ~ "diff_vitesse_elevee"
          )
        ) |>
        dplyr::filter(flag) |>
        dplyr::select(id_transect, WatchLenKm, Duration_h, Observed_kmh, vit_min_kn, vit_max_kn, vit_med_kn, Vit_knots2kmh, pct_diff, in_speed_range, reason)

      if (nrow(w) > 0) results$R10 <- w
    }

    if (all(c("id_transect", "site") %in% names(t_mis))) {
      norm_site2 <- function(x) {
        y <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT"))
        dplyr::case_when(
          y %in% c("debut", "start") ~ "debut",
          y %in% c("fin", "end") ~ "fin",
          TRUE ~ NA_character_
        )
      }

      r11 <- t_mis |>
        dplyr::mutate(site_norm = norm_site2(site)) |>
        dplyr::group_by(id_transect) |>
        dplyr::summarise(
          n_rows = dplyr::n(),
          n_debut = sum(site_norm == "debut", na.rm = TRUE),
          n_fin = sum(site_norm == "fin", na.rm = TRUE),
          issue = dplyr::case_when(
            n_debut == 1 & n_fin == 1 ~ "ok",
            n_debut == 0 & n_fin == 0 ~ "missing_both",
            n_debut == 0 ~ "missing_debut",
            n_fin == 0 ~ "missing_fin",
            n_debut > 1 | n_fin > 1 ~ "duplicate_debut_or_fin",
            TRUE ~ "other"
          ),
          .groups = "drop"
        ) |>
        dplyr::filter(issue != "ok")

      if (nrow(r11) > 0) results$R11 <- r11
    }
  }

  results
}

.cv_rule_desc <- list(
  R1 = c(e = "id_transect NA but in_transect != HORS TRANSECT", f = "id_transect manquant mais in_transect != HORS TRANSECT"),
  R2 = c(e = "code_espece = RIEN but non-NA fields present", f = "code_espece = RIEN mais champs non-NA presents"),
  R3 = c(e = "code_espece != RIEN but nb_individu or activite NA", f = "code_espece != RIEN mais nb_individu ou activite manquant"),
  R4 = c(e = "code_espece != RIEN but missing expected distance data", f = "code_espece != RIEN mais donnees de distance manquantes"),
  R5 = c(e = "_rad data present in _par-only mission", f = "Donnees _rad presentes dans une mission _par seulement"),
  R6 = c(e = "activite=VOL but snapshot != OUI", f = "activite=VOL mais snapshot != OUI"),
  R7 = c(e = "activite=EAU but snapshot != NON", f = "activite=EAU mais snapshot != NON"),
  R8 = c(e = "in_transect or snapshot NA for non-RIEN rows", f = "in_transect ou snapshot manquant pour lignes non-RIEN"),
  R9 = c(e = "_rad and _par values differ in both-protocol mission", f = "Valeurs _rad et _par differentes dans mission protocole double"),
  R10 = c(e = "WatchLenKm / vit_plateforme / duration inconsistency (transects)", f = "Incoherence WatchLenKm / vit_plateforme / duree (transects)"),
  R11 = c(e = "Transect endpoint integrity: missing/duplicate Debut/Fin", f = "Integrite des extremites de transect: Debut/Fin manquant ou duplique")
)

explain_cv_rule <- function(rule, df_violations, mission_id) {
  cat("\n--------------------------------------------\n")
  cat(msg("mission_label"), mission_id, "\n")
  cat(if (.lang == "f") "Regle : " else "Rule  : ", rule, "\n", sep = "")
  cat(if (.lang == "f") "Desc  : " else "Desc  : ", .cv_rule_desc[[rule]][[.lang]], "\n", sep = "")
  cat(if (.lang == "f") "Lignes: " else "Rows  : ", nrow(df_violations), "\n", sep = "")
  cat("--------------------------------------------\n\n")

  view_in_viewer(df_violations, title = paste0(mission_id, " - ", rule, ": ", .cv_rule_desc[[rule]][[.lang]]))

  snippet <- NULL

  if (rule == "R9") {
    cat(if (.lang == "f")
      "Les valeurs _rad et _par different - inspection manuelle requise.\n"
    else
      "_rad and _par values differ - manual inspection required.\n")
  }

  if (rule == "R10") {
    x <- df_violations |>
      dplyr::mutate(
        expected_duration_h = WatchLenKm / Vit_knots2kmh,
        delta_min = (Duration_h - expected_duration_h) * 60,
        abs_pct = abs(pct_diff),
        severity = dplyr::case_when(abs_pct >= 100 ~ "critical", abs_pct >= 50 ~ "high", abs_pct >= 20 ~ "moderate", TRUE ~ "low")
      )

    by_reason <- x |>
      dplyr::count(reason, name = "n") |>
      dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
      dplyr::arrange(dplyr::desc(n))

    cat("R10 = distance / duration / speed inconsistency.\n")
    cat("Interpretation: observed speed is compared against the range of recorded platform speeds within the transect (min-max), using the median speed as reference.\n")
    cat("Threshold: flag when outside speed range (10% tolerance), negative duration, or missing speed info.\n\n")
    print(by_reason, n = Inf)

    detail <- x |>
      dplyr::select(id_transect, reason, severity, WatchLenKm, Duration_h, vit_min_kn, vit_max_kn, vit_med_kn, in_speed_range, expected_duration_h, delta_min, Observed_kmh, Vit_knots2kmh, pct_diff) |>
      dplyr::arrange(dplyr::desc(abs(pct_diff)))
    print(detail, n = min(20, nrow(detail)))

    cat("\nGUIDED ACTIONS:\n")
    cat("1) duree_negative: check swapped Start/End or timestamp error\n")
    cat("2) vit_plateforme_manquante: replace missing vit_plateforme with computed speed (Observed_kmh / 1.852) when acceptable\n")
    cat("3) duree_tres_courte: check time precision/rounding\n")
    cat("4) vit_tres_faible: check units or data entry\n")
    cat("5) diff_vitesse_elevee: targeted manual inspection\n\n")

    ids <- paste(unique(x$id_transect), collapse = ", ")
    if (any(x$reason == "vit_plateforme_manquante", na.rm = TRUE)) {
      ids_miss <- paste(unique(x$id_transect[x$reason == "vit_plateforme_manquante"]), collapse = ", ")
      snippet <- paste0(
        "# R10 -- targeted inspection (mission ", mission_id, ")\n",
        "transects |> filter(mission == \"", mission_id, "\", id_transect %in% c(", ids, ")) |>\n",
        "  select(mission, id_transect, site, date_heure, longitude, latitude, vit_plateforme)\n\n",
        "# R10 -- impute missing vit_plateforme from computed speed (knots)\n",
        "vit_impute <- run_cross_validation(\"", mission_id, "\")$R10 |>\n",
        "  dplyr::filter(reason == \"vit_plateforme_manquante\") |>\n",
        "  dplyr::transmute(id_transect, vit_calc_kn = Observed_kmh / 1.852)\n\n",
        "transects <- transects |>\n",
        "  dplyr::left_join(vit_impute, by = \"id_transect\") |>\n",
        "  dplyr::mutate(vit_plateforme = dplyr::if_else(\n",
        "    mission == \"", mission_id, "\" & id_transect %in% c(", ids_miss, ") & is.na(vit_plateforme),\n",
        "    vit_calc_kn, vit_plateforme\n",
        "  )) |>\n",
        "  dplyr::select(-vit_calc_kn)\n"
      )
    } else {
      snippet <- paste0(
        "# R10 -- targeted inspection (mission ", mission_id, ")\n",
        "transects |> filter(mission == \"", mission_id, "\", id_transect %in% c(", ids, ")) |>\n",
        "  select(mission, id_transect, site, date_heure, longitude, latitude, vit_plateforme)\n"
      )
    }

    cat("SUGGESTED FIX:\n\n")
    cat(snippet, "\n")
    return(invisible(snippet))
  }

  if (rule == "R11") {
    by_issue <- df_violations |>
      dplyr::count(issue, name = "n") |>
      dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
      dplyr::arrange(dplyr::desc(n))

    cat(if (.lang == "f")
      "R11 = verification de l'integrite des extremites de transect (Debut/Fin).\n"
    else
      "R11 = transect endpoint integrity check (Start/End).\n")

    print(by_issue, n = Inf)

    ids <- paste(unique(df_violations$id_transect), collapse = ", ")
    snippet <- paste0(
      "# R11 -- inspect transects with endpoint issues (mission ", mission_id, ")\n",
      "transects |> filter(mission == \"", mission_id, "\", id_transect %in% c(", ids, ")) |>\n",
      "  select(mission, id_transect, site, date_heure, longitude, latitude, vit_plateforme) |>\n",
      "  arrange(id_transect, date_heure)\n\n",
      "# R11 -- auto-create missing Debut/Fin rows for transects with exactly one missing endpoint\n",
      "norm_site <- function(x) {\n",
      "  y <- tolower(iconv(as.character(x), to = \"ASCII//TRANSLIT\"))\n",
      "  dplyr::case_when(\n",
      "    y %in% c(\"debut\", \"start\") ~ \"debut\",\n",
      "    y %in% c(\"fin\", \"end\") ~ \"fin\",\n",
      "    TRUE ~ NA_character_\n",
      "  )\n",
      "}\n\n",
      "t_mis <- transects |>\n",
      "  dplyr::filter(mission == \"", mission_id, "\") |>\n",
      "  dplyr::mutate(site_norm = norm_site(site), dt = suppressWarnings(lubridate::as_datetime(date_heure)))\n\n",
      "status <- t_mis |>\n",
      "  dplyr::group_by(id_transect) |>\n",
      "  dplyr::summarise(\n",
      "    n_debut = sum(site_norm == \"debut\", na.rm = TRUE),\n",
      "    n_fin = sum(site_norm == \"fin\", na.rm = TRUE),\n",
      "    issue = dplyr::case_when(\n",
      "      n_debut == 0 & n_fin > 0 ~ \"missing_debut\",\n",
      "      n_fin == 0 & n_debut > 0 ~ \"missing_fin\",\n",
      "      TRUE ~ \"other\"\n",
      "    ),\n",
      "    .groups = \"drop\"\n",
      "  ) |>\n",
      "  dplyr::filter(issue %in% c(\"missing_debut\", \"missing_fin\"), id_transect %in% c(", ids, "))\n\n",
      "first_rows <- t_mis |>\n",
      "  dplyr::arrange(id_transect, is.na(dt), dt) |>\n",
      "  dplyr::group_by(id_transect) |>\n",
      "  dplyr::slice_head(n = 1) |>\n",
      "  dplyr::ungroup()\n\n",
      "last_rows <- t_mis |>\n",
      "  dplyr::arrange(id_transect, is.na(dt), dplyr::desc(dt)) |>\n",
      "  dplyr::group_by(id_transect) |>\n",
      "  dplyr::slice_head(n = 1) |>\n",
      "  dplyr::ungroup()\n\n",
      "rows_debut <- status |>\n",
      "  dplyr::filter(issue == \"missing_debut\") |>\n",
      "  dplyr::inner_join(first_rows, by = \"id_transect\") |>\n",
      "  dplyr::mutate(site = \"Début\")\n\n",
      "rows_fin <- status |>\n",
      "  dplyr::filter(issue == \"missing_fin\") |>\n",
      "  dplyr::inner_join(last_rows, by = \"id_transect\") |>\n",
      "  dplyr::mutate(site = \"Fin\")\n\n",
      "rows_to_add <- dplyr::bind_rows(rows_debut, rows_fin) |>\n",
      "  dplyr::select(-n_debut, -n_fin, -issue, -site_norm, -dt)\n\n",
      "transects <- dplyr::bind_rows(transects, rows_to_add)\n"
    )

    cat(if (.lang == "f") "CORRECTION SUGGEREE:\n\n" else "SUGGESTED FIX:\n\n")
    cat(snippet, "\n")
    return(invisible(snippet))
  }

  invisible(snippet)
}

run_cv_for_mission <- function(mission_id) {
  cv <- run_cross_validation(mission_id)

  if (length(cv) == 0) {
    cat(if (.lang == "f") "  Aucune violation de validation croisee detectee.\n" else "  No cross-validation violations found.\n")
    return(invisible(NULL))
  }

  rules <- names(cv)
  cat(if (.lang == "f")
    paste0("\n  ", length(rules), " regle(s) violee(s) : ", paste(rules, collapse = ", "), "\n")
  else
    paste0("\n  ", length(rules), " cross-validation rule(s) violated: ", paste(rules, collapse = ", "), "\n"))

  for (rule in rules) {
    df_viol <- cv[[rule]]
    cat(sprintf("\n  [%s] %d rows -- %s\n", rule, nrow(df_viol), .cv_rule_desc[[rule]][[.lang]]))

    choice <- prompt(
      if (.lang == "f") "  Options : [v]oir / [s]auter > " else "  Options: [v]iew / [s]kip > ",
      c("v", "s")
    )

    if (choice == "v") {
      snippet <- explain_cv_rule(rule, df_viol, mission_id)
      if (!is.null(snippet)) {
        post <- prompt(
          if (.lang == "f") "  Options : [a]jouter correction / [Entree] continuer > " else "  Options: [a]dd fix to script / [Enter] continue > ",
          c("a", "")
        )
        if (post == "a") {
          fix_table <- if (rule %in% c("R10", "R11")) "transects" else "observations"
          append_fix(snippet, mission_id, paste0("CV_", rule), fix_table, rule)
        }
      }
    }
  }

  invisible(NULL)
}
